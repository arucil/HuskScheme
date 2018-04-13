{-# LANGUAGE InstanceSigs #-}

module ContT where

import Control.Monad.IO.Class
import Control.Monad.State


newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f (ContT ca) = ContT $ \k ->
    ca $ \a -> k $ f a

instance Applicative (ContT r m) where
  pure :: a -> ContT r m a
  pure a = ContT $ \k -> k a

  (<*>) :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  ContT cf <*> ContT ca = ContT $ \k ->
    cf $ \f ->
      ca $ \a -> k $ f a

instance Monad (ContT r m) where
  (>>=) :: ContT r m a -> (a -> ContT r m b) -> ContT r m b
  ContT ca >>= f = ContT $ \k ->
    ca $ \a ->
      runContT (f a) k

instance MonadTrans (ContT r) where
  lift :: Monad m => m a -> ContT r m a
  lift ma = ContT $ \k -> ma >>= k

instance MonadIO m => MonadIO (ContT r m) where
  liftIO :: IO a -> ContT r m a
  liftIO = lift . liftIO


-----------------------    MonadCont      -------------------

class MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (ContT r m) where
  callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
  callCC f = ContT $ \k ->
    runContT (f $ \a -> ContT $ \_ -> k a) k