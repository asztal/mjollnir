{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Compiler
    ( Compiler
    , runCompiler
    , throwAt
    , withErrorContext
    , module Located
    , module MValue
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError(..), Error(..), ErrorT, runErrorT)
import Control.Monad (ap)

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
    
