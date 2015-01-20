{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Problem2 where
import Control.Monad (when)
import Control.Concurrent.STM

-- Problem 2a

{- See lecture 10:

pseq, par :: a -> b -> b
-- denotational semantics (pseudo-code):
pseq _|_ y = _|_
pseq _    y = y
par thread main = main

-- Operational semantics (informal):
pseq x y: Evaluate first x, and then y

par thread main: Evaluate thread in parallel, and immediately return main
-}


-- Problem 2b
-- From RWH Ch.28: 
newtype Gold = Gold Int 
  deriving (Eq, Ord, Show, Num)
type Balance = TVar Gold
transfer :: Gold -> Balance -> Balance -> IO ()
transfer qty fromBal toBal = atomically $ do
  fromQty <- readTVar fromBal
  when (qty > fromQty) $
    retry
  writeTVar fromBal (fromQty - qty)
  readTVar toBal >>= writeTVar toBal . (qty +)


-- 1c)
{- Because of referential transparency (purity) the STM code would
evaluate in exactly the same way if run again before any variable has
changed. In that way some unneccessary work can be avoided.

It will continue (redo) the computation after any change of an
accessed variable (so also if the first balance has been decreased,
even though that would still lead to a new retry).
-}
