
module Problem5 where

import Control.Concurrent.STM

data LockState = Locked | Unlocked
type Lock = TVar LockState

-- a)

newLock :: STM Lock
newLock = newTVar Unlocked

lock :: Lock -> STM ()
lock l = do
  s <- readTVar l
  case s of
    Locked   -> retry
    Unlocked -> writeTVar l Locked

unlock :: Lock -> STM ()
unlock l = writeTVar l Unlocked

-- b)

criticalSection :: Lock -> IO a -> IO a
criticalSection l m = do
  atomically $ lock l
  x <- m
  atomically $ unlock l
  return x

-- c)

lockAny :: [Lock] -> STM Lock
lockAny [] = retry
lockAny (l:ls) =
  do lock l
     return l
  `orElse`
     lockAny ls

