{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateT where

import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> fmap (mapFst f) $ sma s
    where mapFst g (a, b) = (g a, b)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> return (x, s)

  StateT smf <*> sma = StateT $
    \s -> do
      (f, s') <- smf s
      runStateT (fmap f sma) s'

instance Monad m => Monad (StateT s m) where
  StateT sma >>= f = StateT $
    \s -> do
      (a, s') <- sma s
      runStateT (f a) s'


instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)


instance MonadTrans (StateT s) where
  lift m = StateT $ \s ->
    do
      a <- m
      return (a, s)


instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO