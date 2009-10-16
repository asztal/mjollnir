{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, RecordWildCards #-}

module Compiler
    ( Compiler
    , runCompiler
    , throwAt
    , checkNameClashes
    , withErrorContext
    , syntaxToExp
    , compileFunction
    , module Located
    , module MValue
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError(..), Error(..), ErrorT, runErrorT)
import Control.Monad

import Data.Array.Unboxed (listArray)
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord

import AST
import Eval
import Located
import MValue

------------------------------------------------------------------------------

data CompilerError = CompilerError SrcSpan [String] [ErrorContext]
type ErrorContext = String

instance Error CompilerError where
    noMsg = strMsg "Unknown error (someone called noMsg)"
    strMsg xs = CompilerError noSpan [xs] []

instance Show CompilerError where
    show (CompilerError loc msg ctxs) = show loc ++ ":\n" ++ unlines (map ("  " ++) msg ++ ("":reverse ctxs))

newtype Compiler a = Compiler (ErrorT CompilerError IO a)
    deriving (Functor, Monad, MonadIO, MonadError CompilerError)

instance Applicative Compiler where
    pure = return
    (<*>) = ap

runCompiler :: Compiler a -> IO (Either CompilerError a)
runCompiler (Compiler x) = runErrorT x

withErrorContext ctx action = action `catchError` addCtx where
    addCtx (CompilerError loc msg ctxs) = throwError $ CompilerError loc msg (ctx:ctxs)

throwAt :: MonadError CompilerError m => SrcSpan -> [String] -> m a
throwAt loc lines = throwError $ CompilerError loc lines []

checkNameClashes :: SrcSpan -> String -> [LName] -> Compiler ()
checkNameClashes loc msg xs = case findClashes xs of
    [] -> return ()
    xs -> throwAt loc $ msg : concatMap showClash xs
    where
        findClashes xs = filter (not . null . tail) . group . sort $ xs
        showClash names = [ "  " ++ unLoc name ++ atLoc name | name <- names ]

------------------------------------------------------------------------------
    
syntaxToExp :: Map LVarName Int -> Map LVarName Int -> Map LVarName Int -> [LVarName]
            -> Located Syntax
            -> Compiler (Exp Var Fun)
syntaxToExp localIndices argIndices refArgIndices importedVars (L loc expr) =
    context $ case expr of
        LiteralS lit -> return $ transformLiteral lit
        ListS [] -> return $ ConstE Nil
        ListS (x:xs) -> app (withLoc x ":") [] [x, L loc $ ListS xs] -- Location info isn't perfect here.

        FunRefS name (L _ arity) -> return $ FunE (NamedFun name arity) arity
        VarRefS name -> ReadE <$> lookupVarD name

        GetterS a is -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissækja" ++ show (length is)) (a:is)
        SetterS a is e -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissetja" ++ show (length is)) (a:is ++ [e])
        AssignS name e -> WriteE <$> lookupVarD name <*> recur e

        FunCallS name refs args -> app name refs args
        OperatorS name x y -> app name [] [x, y]
        OperatorCallS name args -> app name [] args
        OperatorUnaryS name x -> app name [] [x]

        AndS x y -> AndE <$> recur x <*> recur y
        OrS x y -> OrE <$> recur x <*> recur y
        NotS x -> NotE <$> recur x

        CaseS cond cases defaultCase -> do
            CaseE
            <$> recur cond
            <*> mapM toBranch cases
            <*> maybe (pure (ConstE Nil)) recurs defaultCase

        IfS cond thenExp elseIfs otherwiseExp ->
            fold $ (cond, thenExp) : elseIfs
            where
                fold [] = maybe (pure (ConstE Nil)) recurs otherwiseExp
                fold ((cond, thenExp) : rest) = IfE <$> recur cond <*> recurs thenExp <*> fold rest

        WhileLoopS cond body -> while cond body
        ForLoopS inits cond incs body -> for inits cond incs body 
        
        LoopS xs -> LoopE . ManyE <$> mapM recur xs

        BlockS [] -> return $ ConstE Nil
        BlockS [x] -> recur x
        BlockS xs -> ManyE <$> mapM recur xs
        
        BreakS -> return BreakE
        ReturnS x -> ReturnE <$> recur x
    
    where
        context = withErrorContext $ "When compiling function at " ++ show loc
    
        recur = syntaxToExp localIndices argIndices refArgIndices importedVars

        recurs [] = return $ ConstE Nil
        recurs [x] = recur x
        recurs xs = ManyE <$> mapM recur xs

        while cond body = do
            break <- IfE <$> recur cond <*> pure (ConstE Nil) <*> pure BreakE
            LoopE . insertBreak break <$> recurs body
            where
                insertBreak break (ManyE xs) = ManyE (break : xs)
                insertBreak break (ConstE Nil) = break
                insertBreak break x = ManyE [break, x]

        for inits cond incs body = do
            inits' <- recurs inits
            body' <- while cond (body ++ incs)
            return . simplifyE $ ManyE [inits', body']

        simplifyE (ManyE xs) = ManyE $ concatMap f xs where
            f (ManyE xs) = xs
            f x = [x]
        simplifyE e = e
        
        app name refs args = AppE
            <$> pure (possiblyFun name (genericLength refs, genericLength args))
            <*> mapM toRefVar refs
            <*> mapM recur args

        toRefVar (L l e) = case e of
            VarRefS name -> Right <$> lookupVarD name
            x -> Left <$> recur (L l x)

        lookupVarD name = 
            maybe (tryImport name) return $ lookupVar name

        tryImport name
            | name `elem` importedVars = return $ NamedVar name
            | otherwise = throwAt (getLoc name) [ "Unbound variable " ++ show name ++ " used (did you mean to import it?)" ]

        lookupVar name = msum $ zipWith find
                [localIndices, argIndices, refArgIndices]
                [LocalVar, ArgVar, RefArgVar]
            where
                find vars constr = constr <$> M.lookup name vars

        possiblyFun name arity = maybe (Right (NamedFun name arity)) Left $ lookupVar name

        transformLiteral (CharLit c) = ConstE $ Word (fromIntegral (ord c))
        transformLiteral (NatLit i) = ConstE $ Word (fromIntegral i)
        transformLiteral (IntLit i) = ConstE $ Word (fromIntegral i)
        transformLiteral (FloatLit f) = ConstE $ Real f
        transformLiteral (StringLit xs) = StrE $ let len = length xs in
            listArray (0, len) (fromIntegral len : map (fromIntegral . ord) xs)

        toBranch (ranges, body) = do
            bodyExp <- recurs body
            return (map toRange ranges, bodyExp)
            
        toRange (L _ (from, to)) =
            (literalAsWord16 from, literalAsWord16 $ fromMaybe from to)

        literalAsWord16 (CharLit c) = fromIntegral (ord c)
        literalAsWord16 (NatLit i) = fromIntegral i
        literalAsWord16 _ = error "Literal in case range wasn't CharLit/NatLit"

compileFunction :: Located FunctionDecl -> Compiler (Function Var Fun)
compileFunction (L funLoc FunctionDecl {..}) = context $ do
    checkVarNames $ fnInOutParams ++ fnInParams ++ concatMap variableDeclNames fnVariables

    body <- compileBody
    
    return $ Function
        (genericLength fnInOutParams, genericLength fnInParams)
        (length allLocals)
        (length fnInParams)
        body

    where
        context = withErrorContext $ "When compiling function at " ++ show funLoc
    
        compileBody = do
            bodyExps <- mapM toExp fnBody
            initExps <- compileInitializers
            return $ simplifyE (ManyE (initExps ++ bodyExps))

        simplifyE (ManyE []) = ConstE Nil
        simplifyE (ManyE [x]) = x
        simplifyE x = x

        -- Convert the initializer list to a list of statements to be inserted
        -- before the function body.
        compileInitializers = catMaybes <$> (sequence $ zipWith f [0..] allLocals) where
            f i (_, Just init) = Just . WriteE (LocalVar i) <$> toExp init
            f _ (_, Nothing) = return Nothing

        (allLocals, importedVars) = (concat *** concat) . partitionEithers $ map f fnVariables where
            f (LocalVarDecl xs) = Left xs
            f (ImportVarDecl xs) = Right xs

        checkVarNames xs = checkNameClashes funLoc "Variable declared multiple times: " xs

        argMap = indexMap fnInParams
        refArgMap = indexMap fnInOutParams
        localMap = indexMap locals

        indexMap = M.fromList . sortBy (comparing snd) . (`zip` [0..])

        (locals, _) = partitionEithers $ concatMap f fnVariables where
            f (LocalVarDecl xs) = map (Left . fst) xs
            f (ImportVarDecl names) = map Right names

        toExp = syntaxToExp localMap argMap refArgMap importedVars
