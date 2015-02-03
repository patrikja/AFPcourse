
module Problem2 where

import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) }

-- a)

instance Monoid w => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= f  = Writer (y, mappend w1 w2)
    where
      (x, w1) = runWriter m
      (y, w2) = runWriter (f x)

-- b)

tell :: w -> Writer w ()
tell w = Writer ((), w)

listen :: Writer w a -> Writer w (a, w)
listen m = Writer ((x, w), w)
  where
    (x, w) = runWriter m

