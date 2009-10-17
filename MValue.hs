{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module MValue
    ( MValue (..)
    , NewMValue (..)
    , MArrayElem (..)
    , MonadST (..)
    , ($=), ($~)
    , ($!=), ($!~)
    , OrdIORef
    , module Control.Monad.ST
    , module Data.IORef
    , module Data.STRef
    , module Data.Array.IO
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ST
import Data.Array.IO
import Data.IORef
import Data.STRef
import Data.Unique

-- | A writeable area of memory that may be part of a larger mutable data structure.
-- | Minimal complete definition readMValue and one of modifyMValue and writeMValue
class Monad m => MValue m r a | r -> a where
    writeMValue :: r -> a -> m ()
    readMValue :: r -> m a
    modifyMValue :: r -> (a -> a) -> m ()

    modifyMValue ref f = writeMValue ref . f =<< readMValue ref
    writeMValue ref x = modifyMValue ref (const x)

class Monad m => NewMValue m r a | r -> a where
    newMValue :: a -> m r

instance MonadIO m => MValue m (IORef a) a where
    writeMValue r x = liftIO (writeIORef r x)
    readMValue r = liftIO (readIORef r)

instance MonadIO m => NewMValue m (IORef a) a where
    newMValue x = liftIO (newIORef x)

class Monad m => MonadST s m | m -> s where
    liftST :: ST s a -> m a

instance MonadST s (ST s) where liftST = id

instance MonadST s m => MValue m (STRef s a) a where
    writeMValue r x = liftST $ writeSTRef r x
    readMValue r = liftST $ readSTRef r

instance MonadST s m => NewMValue m (STRef s a) a where
    newMValue x = liftST (newSTRef x)

data MArrayElem a i e = MArrayElem (a i e) i

instance (Ix i, MArray a e m) => MValue m (MArrayElem a i e) e where
    readMValue (MArrayElem arr ix) = readArray arr ix
    writeMValue (MArrayElem arr ix) = writeArray arr ix

-- IORefs that are also ordered, so they can be placed in Sets, etc.
data OrdIORef a = OrdIORef Unique (IORef a) deriving Eq

instance Ord (OrdIORef a) where
    compare (OrdIORef u _) (OrdIORef v _) = compare u v

instance MonadIO m => MValue m (OrdIORef a) a where
    readMValue (OrdIORef _ r) = readMValue r
    writeMValue (OrdIORef _ r) x = writeMValue r x

instance MonadIO m => NewMValue m (OrdIORef a) a where
    newMValue x = OrdIORef `liftM` liftIO newUnique `ap` newMValue x

($=), ($!=) :: MValue m r a => r -> a -> m ()
($~), ($!~) :: MValue m r a => r -> (a -> a) -> m ()

r $= x = writeMValue r x
r $~ f = modifyMValue r f
r $!= x = writeMValue r $! x
r $!~ f = (r $!=) . f =<< readMValue r

