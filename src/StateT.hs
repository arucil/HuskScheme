{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module StateT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    (\(a, b) -> (f a, b)) <$> sma s

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> return (x, s)

  StateT smf <*> sma = StateT $ \s -> do
    (f, s') <- smf s
    runStateT (fmap f sma) s'

instance Monad m => Monad (StateT s m) where
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return $ (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO


evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st s = fst <$> runStateT st s

-----------------      MonadState     ----------------

class Monad m => MonadState s m | m -> s where
  {-# MINIMAL state | get, put #-}

  get :: m s
  get = do
    s <- state $ \s -> (s, s)
    return s

  put :: s -> m ()
  put s = state $ \_ -> ((), s)

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let (a, s') = f s
    put s'
    return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = do
  s <- get
  put $ f s