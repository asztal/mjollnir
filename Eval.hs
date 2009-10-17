{-# LANGUAGE GADTs, RankNTypes, EmptyDataDecls, MultiParamTypeClasses,
             FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, 
             GeneralizedNewtypeDeriving, DeriveDataTypeable, NamedFieldPuns
             #-}

module Eval where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow (left)
import qualified Control.Exception as E
import Control.Monad (ap, join, liftM, liftM2, forever, when)
import Control.Monad.Trans (MonadIO (..), lift)
import Control.Monad.Cont (ContT, MonadCont, runContT, callCC)
import Control.Monad.State (StateT, MonadState (..), gets, modify, evalStateT)

import Data.Convertible (Convertible (..))
import Data.List (genericLength)
import Data.Word (Word16, Word8)

import Data.Array.Unboxed (Array, listArray, (!))
import Data.Typeable (Typeable)

import AST
import Exp
import MValue

data WordTag
data RealTag
data StrTag
data ArrayTag
data FunTag
data PairTag
data NilTag

data Value
    = Word !Word16
    | Real !Double
    | Str (IOUArray Int Word8)
    | Array (IOArray Int Value)
    | Fun Fun
    | Pair (IORef Value) (IORef Value)
    | Nil
    deriving Typeable

-- Although there are many types of literal, and conceptually two types of integer
-- (signed and unsigned) all integers are stored as 16-bit words. It is only
-- functions that differentiate between signed and unsigned integers.
--  * Word: 16-bit integral values. All integral literals ('a', 500, -324) are Words.
--  * Real: Floating-point values such as 453.43.
--  * Str: String values such as "Sussman"
--  * Pair: A cons cell. It is displayed as a list. Improper lists are displayed as [1,2,3:4].
--  * Nil: The empty list, which doubles up as the false value (true is 1).
--  * Fun: A function. The arity of a function is always known.
instance Show Value where
    show (Word w) = show w
    show (Real f) = show f
    show (Str _) = "<string>"
    show (Array _) = "<array>"
    show (Pair _ _) = "[..]"
    show (Fun _) = "stef(?)"
    show Nil = "[]"

------------------------------------------------------------------------------

class MShow m a where
    mshow :: a -> m String

instance MonadIO m => MShow m Value where
    mshow (Word w) = return (show w)
    mshow (Real f) = return (show f)
    mshow (Str s) = liftM (('"':) . (++"\"") . map (toEnum . fromIntegral) . drop 1) (liftIO (getElems s))
    mshow (Array xs) = do
        b <- liftIO $ getBounds xs
        return $ "<array: length = " ++ show (rangeSize b) ++ ">"
    mshow (Pair h t) = liftIO $ mshowList True h t where
        mshowList isFirst x xs = do
            x' <- mshow =<< readMValue x
            xs' <- readMValue xs
            let prefix = if isFirst then "[" else ""
            let suffix = if isFirst then "" else "]"
            case xs' of
                Nil -> return $ prefix ++ x' ++ "]"
                Pair a b -> do
                    rest <- mshowList False a b
                    return $ prefix ++ x' ++ "," ++ rest
                _ -> do
                    rest <- mshow xs'
                    -- Oddly, it uses lisp syntax for improper lists.
                    return $ x' ++ " . " ++ rest ++ suffix
    mshow (Fun _) = return $ "stef(?)"
    mshow Nil = return "[]"

instance Convertible Value Bool where
    safeConvert Nil = return False
    safeConvert _ = return True

instance Convertible Bool Value where
    safeConvert False = return Nil
    safeConvert True = return (Word 1)

------------------------------------------------------------------------------

data StackFrame = StackFrame
    { refArguments :: Array Int Var
    , arguments :: IOArray Int Value
    , localVars :: IOArray Int Value
    }

-- Two continuations are used: the "return" continuation, to exit the current procedure,
-- and the "break" continuation, used to exit the current loop.
-- The return and break continuations correspond to the skila and út keywords, respectively.
data ExecState = ExecState
    { stackFrames :: [StackFrame]
    
    -- These may "return" a Value, but they will never actually produce that value.
    , returnCont :: Value -> Eval Value
    , breakCont :: Value -> Eval Value -- the Value should be Nil.
    }

pushStackFrame :: [Var] -> [Value] -> Int -> Eval ()
pushStackFrame refs args locals = do
    let refsArr = listArray (0,length refs-1) refs
    argsArr <- liftIO $ newListArray (0,length args-1) args
    localsArr <- liftIO $ newArray (0, locals-1) Nil
    Eval . modify $ \s ->
        s { stackFrames = StackFrame refsArr argsArr localsArr : stackFrames s }

initialExecState :: ExecState
initialExecState = ExecState
    []
    (const $ throw "skila outside of function call")
    (const $ throw "út outside of loop")

getsStack :: (StackFrame -> a) -> Eval a
getsStack f = do
    frame <- Eval (gets stackFrames)
    case frame of
        [] -> throw "No stack frame!"
        (s:_) -> return (f s)

------------------------------------------------------------------------------

newtype Eval a = Eval (ContT Value (StateT ExecState IO) a)
    deriving (Functor, Monad, MonadIO, MonadCont)

instance Applicative Eval where
    pure = return
    (<*>) = ap

runEval :: Eval Value -> IO (Either String Value)
runEval (Eval m) = try . flip evalStateT initialExecState . flip runContT return $ m

liftState :: StateT ExecState IO Value -> Eval Value
liftState x = Eval (lift x)

runToState :: Eval Value -> StateT ExecState IO Value
runToState (Eval s) = flip runContT return $ s

-- The problem with withStateT is that the state isn't actually returned to a
-- previous value if the continuation is invoked.
callCCWithState :: ((Value -> Eval Value) -> ExecState -> ExecState) -> Eval Value -> Eval Value
callCCWithState f m = do
    prevState <- Eval get
    val <- callCC $ \c -> do
        Eval $ put (f c prevState)
        m
    Eval $ put prevState
    return val

-- Sets the break continuation and evaluates the loop.
runLoop :: Eval Value -> Eval Value
runLoop = callCCWithState (\newBreakCont st -> st { breakCont = newBreakCont })

data EvalException = EvalException
    { errorMessage :: String }
    deriving (Show, Typeable)
instance E.Exception EvalException

throw :: String -> Eval a
throw msg = E.throw (EvalException msg)

try :: IO a -> IO (Either String a)
try x = left errorMessage <$> E.try x

------------------------------------------------------------------------------

data Var
    = NamedVar LVarName
    | ResolvedVar (IORef Value)
    | ClosedVar (IOArray Int Value) !Int
    | LocalVar !Int
    | ArgVar !Int
    | RefArgVar !Int

data Function v f = Function
    { funArity :: Arity
    , funLocalCount :: Int
    , funArgCount :: Int
    , funBody :: Exp v f
    }

data Fun
    = NamedFun LFunName Arity
    | NativeFun Arity ([Var] -> [Value] -> Eval Value)
    | ResolvedFun Arity (IORef (Function Var Fun))

instance MValue Eval Var Value where
    readMValue (NamedVar n) = throw $ "Read from NamedVar: " ++ show n
    readMValue (ResolvedVar r) = readMValue r
    readMValue (ClosedVar a i) = liftIO $ readArray a i
    readMValue (LocalVar i) = liftIO . flip readArray i =<< getsStack localVars
    readMValue (ArgVar i) = liftIO . flip readArray i =<< getsStack arguments
    readMValue (RefArgVar i) = readMValue =<< getsStack ((! i) . refArguments)

    writeMValue (NamedVar n) _ = throw $ "Write to NamedVar: " ++ show n
    writeMValue (ResolvedVar r) x = writeMValue r x
    writeMValue (ClosedVar a i) x = liftIO $ writeArray a i x
    writeMValue (LocalVar i) x = (\a -> liftIO $ writeArray a i x) =<< getsStack localVars
    writeMValue (ArgVar i) x = (\a -> liftIO $ writeArray a i x) =<< getsStack arguments
    writeMValue (RefArgVar i) x = flip writeMValue x =<< getsStack ((! i) . refArguments)

------------------------------------------------------------------------------

evalE :: Exp Var Fun -> Eval Value

evalE (NilE) = return Nil
evalE (WordE w) = return (Word w)
evalE (RealE r) = return (Real r)
evalE (StrE arr) = liftIO $ Str <$> thaw arr
evalE (FunE fun _) = return $ Fun fun
evalE (ReadE var) = readMValue var
evalE (WriteE var x) = do
    val <- evalE x
    writeMValue var val
    return val
evalE (AppE (Left var) refs args) = do
    fun <- readMValue var
    case fun of
        Fun f -> join $ apply f <$> evalRefArgs refs <*> mapM evalE args
        _ -> throw $ "Expected function in function application, but got " ++ show fun
evalE (AppE (Right f) refs args) =
    join $ apply f <$> evalRefArgs refs <*> mapM evalE args
evalE (ManyE []) = return Nil
evalE (ManyE [x]) = evalE x
evalE (ManyE (x:xs)) = evalE x >> evalE (ManyE xs)
evalE (AndE x y) = do
    val <- evalE x
    case val of
        Nil -> return val
        _ -> evalE y -- I checked, this is how it works.
evalE (OrE x y) = do
    val <- evalE x
    case val of
        Nil -> evalE y
        _ -> return val
evalE (NotE x) = ifTrue (evalE x) (return (Word 1)) (return Nil)
evalE (LoopE x) = runLoop $ forever (evalE x)
evalE (ReturnE x) = join $ liftM2 ($) (Eval $ gets returnCont) (evalE x)
evalE BreakE = ($ Nil) =<< Eval (gets breakCont)

evalE (CaseE scrutinee branches defaultBranch) = do
    value <- evalE scrutinee
    case value of
        Word x -> tryBranch x branches
        _ -> evalE defaultBranch

    where
        tryBranch _ [] = evalE defaultBranch
        tryBranch x ((ranges,body):rest) = if any (`inRange` x) ranges
            then evalE body
            else tryBranch x rest
    
evalE (IfE cond true false) = ifTrue (evalE cond) (evalE true) (evalE false)

apply :: Fun -> [Var] -> [Value] -> Eval Value
apply (NamedFun n _) _ _ = throw $ "Application of NamedFun: " ++ show n
apply (ResolvedFun arity r) refs args = do
    fun <- readMValue r
    let givenArity = (genericLength refs, genericLength args)
    when (givenArity /= arity) . throw $
        "Function call with wrong argument count: given " ++ show givenArity ++ " instead of " ++ show arity

    -- Set the return continuation.
    callCCWithState (\newReturnCont st -> st { returnCont = newReturnCont }) $ do
        pushStackFrame refs args (funLocalCount fun)
        evalE (funBody fun)

-- Arity is ignored here, because functions should call arityError anyway.
apply (NativeFun _ f) refs args = f refs args

-- When passing an /expression/ to a function as a ref parameter, its
-- value has to be determined and then boxed in an IORef. This way,
-- the function can modify it, but it has no effect on the rest of the program
-- state.
evalRefArgs :: [Either (Exp Var Fun) Var] -> Eval [Var]
evalRefArgs xs = mapM toVar xs
    where
        toVar (Left e) = ResolvedVar <$> (liftIO . newIORef =<< evalE e)
        toVar (Right v) = close v

        close (LocalVar i) = ClosedVar <$> getsStack localVars <*> pure i
        close (ArgVar i) = ClosedVar <$> getsStack arguments <*> pure i
        close (RefArgVar i) = getsStack ((! i) . refArguments)
        close v = return v

ifTrue :: Eval Value -> Eval Value -> Eval Value -> Eval Value
ifTrue mx t f = mx >>= \v -> case v of Nil -> f; _ -> t
