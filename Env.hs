{-# LANGUAGE PatternGuards #-}

module Env
    ( buildEnv
    , Env
    , emptyEnv
    ) where

import Control.Arrow (first, second)
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.State as SM
import Control.Monad (join, when, foldM)
import Control.Monad.Trans (lift)
import Control.Monad.Error (MonadError(..))

import qualified Data.Map as M
import Data.Map (Map)

import AST
import Compiler
import Eval
import Module
import MValue
import Kjarni

------------------------------------------------------------------------------

newtype Env = Env (Map Name Module)

------------------------------------------------------------------------------

emptyEnv :: Env
emptyEnv = Env M.empty

singletonEnv :: Name -> Module -> Env
singletonEnv n m = Env (M.singleton n m)

combineEnv :: Env -> Env -> Compiler Env
combineEnv (Env left) (Env right)
    | (x:_) <- M.keys (M.intersection left right)
        = throwError $ "Top-level environment already has a module named " ++ show x
    | otherwise
        = return . Env $ M.union left right

lookupEnv :: Name -> Env -> Maybe Module
lookupEnv name (Env e) = M.lookup name e

------------------------------------------------------------------------------

-- TODO: Separate KJARNI into whatever submodules it's supposed to be made of.
--       (
defaultEnv :: Compiler Env
defaultEnv = do
    k <- double <$> kjarni
    let g = k
        felakjar = fela k
        felagrun = fela g

    foldM combineEnv emptyEnv
        [ singletonEnv "KJARNI" k
        , singletonEnv "GRUNNUR" k
        , singletonEnv "FELAGRUN" felagrun
        , singletonEnv "FELAKJAR" felakjar
        ]

    where
        double (Module m) = Module $ m `M.union` M.mapKeys parenize m
        fela (Module m) = Module $ M.mapWithKey (\k _ -> ExportAgain $ parenize k) m
        parenize x = "(" ++ x ++ ")"

type EnvS a = SM.StateT ([(Name, Fun)], Env) Compiler a

buildEnv :: Program -> Compiler ([(Name, Fun)], Env)
buildEnv xs = do
    e <- defaultEnv
    SM.execStateT buildEnv' ([], e)
    where
        buildEnv' = mapM_ perform xs

        perform :: ProgramStatement -> EnvS ()
        perform (ModuleAssign name mexpr) = do
            m <- eval mexpr
            currentEnv <- SM.gets snd
            e' <- lift $ combineEnv (singletonEnv name m) currentEnv
            SM.modify $ second (const e')

        perform (ModuleFrom name member mexpr) = do
            m <- eval mexpr
            imports <- lift (moduleImports m)
            when (not (null imports)) $ do
                throwError $ "The following imports were not resolved when compiling entry point " ++ show name ++ ":\n" ++ unlines
                    [ "  " ++ n ++ " :: " ++ show t | (n, t) <- imports ]
            
            case lookupExport member m of
                Nothing -> throwError $
                     "Undeclared function bound by top-level program declaration " ++ show name ++ ": " ++ show member
                Just (ExportNative arity fun) -> SM.modify $ first ((name, NativeFun arity fun) :)
                Just (ExportFun ref) -> do
                    fun <- readMValue ref
                    SM.modify $ first ((name, ResolvedFun (funArity fun) ref) :)
                Just (ExportVar _) -> throwError $
                    "Error: variable " ++ show member ++ " was bound to top-level program declaration " ++ show name
                Just (ExportAgain n) -> throwError $
                    "Error: unresolved export (" ++ member ++ " -> " ++ n ++ ") was bound to top-level program declaration " ++ show name

        eval :: ModuleDecl -> EnvS Module
        eval (NamedModule name) = do
            m <- lookupEnv name <$> SM.gets snd
            maybe (throwError $ "Module not found: " ++ show name) return m

        eval (ModuleDecl exports) =
            lift $ singleModule exports

        eval (RecursiveModule mexpr) =
            lift . iterateModule =<< eval mexpr

        eval (CombinedModule left "+" right) =
            join $ (lift . ) . plusModule <$> eval left <*> eval right

        eval (CombinedModule left "*" right) =
            join $ (lift . ) . composeModule <$> eval left <*> eval right

        eval (CombinedModule left ":" right) =
            join $ (lift . ) . combineModule <$> eval left <*> eval right

        eval (CombinedModule left "&" right) =
            eval (RecursiveModule (CombinedModule left "+" right))
            
        eval (CombinedModule _ op _) =
            throwError $ "Unknown module operator: " ++ show op
