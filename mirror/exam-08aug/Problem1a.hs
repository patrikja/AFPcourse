
module Problem1a where

class Monad m => GameMonad m where
  extraLife  :: m ()
  getLives   :: m Int
  checkPoint :: m a -> m a
  die        :: m a

newtype Game a = Game { unGame :: Int -> Result a }

type Result a = Either Int (a, Int)
-- Alternative definitions:
-- type Result a = (Maybe a, Int)
-- data Result a = GameOver | Died Int | Survived a Int

instance Monad Game where
  return x = Game $ \n -> Right (x, n)
  m >>= f  = Game $ \n -> case unGame m n of
    Left n       -> Left n
    Right (x, n) -> unGame (f x) n

instance GameMonad Game where
  extraLife    = Game $ \n -> Right ((), n + 1)
  getLives     = Game $ \n -> Right (n, n)
  die          = Game $ \n -> Left (n - 1)
  checkPoint m = Game $ \n -> case unGame m n of
    Left n | n > 0     -> unGame (checkPoint m) n
           | otherwise -> Left 0
    Right (x, n)       -> Right (x, n)

runGame :: Game a -> Int -> Maybe (a, Int)
runGame m n = case unGame m n of
  Left _  -> Nothing
  Right r -> Just r

