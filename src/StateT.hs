{-# LANGUAGE TupleSections #-}

module StateT
  (
    StateT(..)
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ fmap (fmap (mapFst f)) sma

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)

  StateT smf <*> StateT sma = StateT $
    \s -> do
      (f, s') <- smf s
      (a, s'') <- sma s'
      return (f a, s'')

instance Monad m => Monad (StateT s m) where
  StateT sma >>= f = StateT $
    \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> (,s) <$> ma

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO