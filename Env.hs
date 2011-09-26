{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving #-}

module Env
    ( Env
    , EnvM, runEnv, runDefaultEnv
    , perform
    , emptyEnv
    ) where

import Control.Arrow
import Control.Applicative
import qualified Control.Monad.State as SM
import Control.Monad
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Error (MonadError(..))

import qualified Data.Map as M
import Data.Map (Map)

import AST
import Compiler
import Eval
import Module
import Kjarni
import Var

------------------------------------------------------------------------------

newtype Env r v f = Env (Map LName (Module r v f))

------------------------------------------------------------------------------

emptyEnv :: Env r v f
emptyEnv = Env M.empty

singletonEnv :: LName -> Module r v f -> Env r v f
singletonEnv n m = Env (M.singleton n m)

combineEnv :: Env r v f -> Env r v f -> Either String (Env r v f)
combineEnv (Env left) (Env right)
    | (x:_) <- M.keys (M.intersection left right)
        = throwError $ "Top-level environment already has a module named " ++ show x
    | otherwise
        = return . Env $ M.union left right

lookupEnv :: LName -> Env r v f -> Maybe (Module r v f)
lookupEnv name (Env e) = M.lookup name e

------------------------------------------------------------------------------

newtype EnvM r v f a = EnvM (SM.StateT ([(LName, f)], Env r v f) Compiler a)
    deriving (MonadIO, Monad, Functor, Applicative, MonadError CompilerError, SM.MonadState ([(LName, f)], Env r v f))

runDefaultEnv (EnvM m) = do
    e <- defaultEnv
    SM.execStateT m ([], e)

runEnv e (EnvM m) = SM.execStateT m ([], e)

------------------------------------------------------------------------------

defaultEnv :: Compiler (Env (IORef Value) IVar IFun)
defaultEnv = context $ do
    [ kjarni, ut, strengir, snua, skrifalin, lesalinu, inn ] <- sequence
        [ double <$> kjarni, ut, strengir, snua, skrifalin, lesalinu, inn ]
    
    grunnur <- foldM plusModule kjarni [ut, strengir, snua, skrifalin, lesalinu]
    inn <- plusModule lesalinu inn -- both LESALINU and INN export "lesalínu"

    -- Make libraries which re-export the base libaries, but with parentheses. 
    -- "FELAKJAR" = {
    --   ! -> (!)
    --   % -> (%)
    --   ...
    --   vistfang -> (vistfang)
    -- }
    -- etc. Since it's not possible to export a name with
    -- parentheses from the language itself, linking a module with FELAGRUN
    -- or FELAKJAR means that the names definitely refer to KJARNI or
    -- GRUNNUR.
    let felakjar = fela kjarni
        felagrun = fela grunnur

    let env' = foldM combineEnv emptyEnv
            [ singletonEnv (genLoc "KJARNI") kjarni
            , singletonEnv (genLoc "UT") ut
            , singletonEnv (genLoc "STRENGIR") strengir
            , singletonEnv (genLoc "SNUA") snua
            , singletonEnv (genLoc "SKRIFALIN") skrifalin
            , singletonEnv (genLoc "LESALINU") lesalinu
            , singletonEnv (genLoc "INN") inn
            , singletonEnv (genLoc "GRUNNUR") grunnur
            , singletonEnv (genLoc "FELAGRUN") felagrun
            , singletonEnv (genLoc "FELAKJAR") felakjar
            ]
    case env' of
        Left xs -> throwAt noSpan ["Internal error: default environment not consistent", xs]
        Right a -> return a

    where
        context = withErrorContext $ "When initializing the environment"
        double (Module loc m) = Module loc $ m `M.union` M.mapKeys parenize m
        fela (Module loc m) = Module loc $ M.mapWithKey (\k _ -> ExportAgain $ parenize k) m
        parenize (L l x) = L l $ "(" ++ x ++ ")"

perform :: FunR r v f => Program -> EnvM r v f ()
perform xs =
    mapM_ perform' xs
    where
        liftC = EnvM . lift
    
        perform' (L loc (ModuleAssign name mexpr)) = context $ do
            m <- eval mexpr
            currentEnv <- SM.gets snd
            e' <- liftC $ case combineEnv (singletonEnv name m) currentEnv of
                Left err -> throwAt loc [err]
                Right x -> return x
            SM.modify $ second (const e')
            where
                context = withErrorContext $ "When compiling named module " ++ show name ++ " at " ++ show loc

        perform' (L loc (ModuleFrom name member mexpr)) = context $ do
            m <- eval mexpr
            imports <- liftC $ moduleImports m
            when (not (null imports)) . liftC . throwAt loc $
                    ("The following imports were not resolved when compiling entry point " ++ show name)
                    : [ "  " ++ n ++ " :: " ++ show t ++ showSpan loc | (L loc n, t) <- imports ]
            
            case lookupExport member m of
                Nothing -> throwAt (getLoc member) 
                     [ "Undeclared function bound by top-level program declaration " ++
                       show name ++ ": " ++ show member
                     ]
                Just (ExportFun fun) -> SM.modify $ first ((name, fun) :)
                Just (ExportVar _) -> throwAt (getLoc member)
                    [ "Error: variable " ++ show member ++
                      " was bound to top-level program declaration " ++ show name
                    ]
                Just (ExportAgain n) -> throwAt (getLoc member)
                    [ "unresolved export (" ++ unLoc member ++ " -> " ++ unLoc n ++
                      ") was bound to top-level program declaration " ++ show name
                    ]

            where
                context = withErrorContext $ "When compiling entry point " ++ show name ++ " at " ++ show loc
                showSpan sp
                    | knownSpan sp = " (at " ++ show sp ++ ")"
                    | otherwise = ""

        eval (L loc (NamedModule name)) = do
            m <- lookupEnv name <$> SM.gets snd
            maybe (throwAt loc [ "Module " ++ show name ++ " not found" ]) return m

        eval (L _ (ModuleDecl exports)) =
            liftC $ singleModule exports

        eval (L _ (RecursiveModule mexpr)) =
            liftC . iterateModule =<< eval mexpr

        eval (L _ (CombinedModule left (L _ "+") right)) =
            join $ (liftC . ) . plusModule <$> eval left <*> eval right

        eval (L _ (CombinedModule left (L _ "*") right)) =
            join $ (liftC . ) . composeModule <$> eval left <*> eval right

        eval (L _ (CombinedModule left (L _ ":") right)) =
            join $ (liftC . ) . combineModule <$> eval left <*> eval right

        eval (L loc (CombinedModule left (L loc' "&") right)) =
            eval (L loc $ RecursiveModule ((L loc $ CombinedModule left (L loc' "+") right)))
            
        eval (L loc (CombinedModule _ (L loc' op) _)) =
            throwAt loc [ "Unknown module operator " ++ show op ++ " at " ++ show loc' ]
