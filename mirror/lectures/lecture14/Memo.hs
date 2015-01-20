
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List  (unfoldr)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef)

-- First the non-recursive core of the fibonacci function
fibcore :: (Eq t, Num t, Num a) => (t -> a) -> t -> a
fibcore cont = \n -> case n of
  0 -> 0
  1 -> 1
  n -> cont (n - 1) + cont (n - 2)

-- then a pure "memoizer":
memoPure :: (Enum a, Num a) => (a -> b) -> Int -> b
memoPure f = let fs = map f [0..]
             in (fs!!)

fibPure :: Int -> Integer
fibPure = memoPure $ fibcore fibPure

-- A completely different "hand-optimized" fib based on 
--   unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 	
-- defined in Data.List
fibUnfoldr :: (Num t) => Int -> t
fibUnfoldr = 
  let fiblist = unfoldr (\(f1,f2) -> Just (f1,(f2,f1+f2))) (0,1)
  in (fiblist !!)

----------------------------------------------------------------
-- Start of impure part
-- memo keeps a local cache of all the previous calls
memo :: Ord a => (a -> b) -> a -> b
memo f = unsafePerformIO $ do
  history <- newIORef Map.empty
  return (f' history)
  where
    f' history x = unsafePerformIO $ do
      tbl <- readIORef history
      case Map.lookup x tbl of
        Just y  -> return y
        Nothing -> do
          let y = f x
          writeIORef history (Map.insert x y tbl)
          return y


-- By memoizing the naive implementation of the Fibonacci
-- numbers we get an efficient version.
fib :: Int -> Integer
fib = memo $ fibcore fib

-- You need to be a little careful with sharing to make sure
-- that you get a single memo structure and not one for each
-- call. For instance, the following doesn't work, since memo
-- won't be applied until there is an argument to badfib.
badfib n = memo (fibcore badfib) n

badfib' n = memoPure (fibcore badfib') n

