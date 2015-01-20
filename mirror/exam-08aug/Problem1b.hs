
module Problem1b where

import Control.Monad.Trans

class Monad m => GameMonad m where
  extraLife  :: m ()
  getLives   :: m Int
  checkPoint :: m a -> m a
  die        :: m a

newtype GameT m a = GameT { unGameT :: Int -> m (Result a) }

type Result a = Either Int (a, Int)

instance Monad m => Monad (GameT m) where
  return x = GameT $ \n -> return $ Right (x, n)
  m >>= f  = GameT $ \n -> do
    r <- unGameT m n
    case r of
      Left n       -> return $ Left n
      Right (x, n) -> unGameT (f x) n

instance MonadTrans GameT where
  lift m = GameT $ \n -> do
    x <- m
    return $ Right (x, n)

instance MonadIO m => MonadIO (GameT m) where
  liftIO = lift . liftIO

instance Monad m => GameMonad (GameT m) where
  extraLife    = GameT $ \n -> return $ Right ((), n + 1)
  getLives     = GameT $ \n -> return $ Right (n, n)
  die          = GameT $ \n -> return $ Left (n - 1)
  checkPoint m = GameT $ \n -> do
    r <- unGameT m n
    case r of
      Left n | n > 0     -> unGameT (checkPoint m) n
             | otherwise -> return $ Left 0
      Right (x, n)       -> return $ Right (x, n)

runGameT :: Monad m => GameT m a -> Int -> m (Maybe (a, Int))
runGameT m n = do
  r <- unGameT m n
  case r of
    Left _  -> return Nothing
    Right r -> return $ Just r

