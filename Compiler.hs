{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler
    ( Compiler
    , runCompiler
    , module MValue
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError(..), ErrorT, runErrorT)
import Control.Monad (ap)

import MValue

------------------------------------------------------------------------------

newtype Compiler a = Compiler (ErrorT String IO a)
    deriving (Functor, Monad, MonadIO, MonadError String)

instance Applicative Compiler where
    pure = return
    (<*>) = ap

runCompiler :: Compiler a -> IO (Either String a)
runCompiler (Compiler x) = runErrorT x
