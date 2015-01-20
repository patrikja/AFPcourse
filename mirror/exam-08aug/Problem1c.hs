{-# LANGUAGE TypeSynonymInstances #-}

module Problem1c where

import Control.Monad.State
import Control.Monad.Error

class Monad m => GameMonad m where
  extraLife  :: m ()
  getLives   :: m Int
  checkPoint :: m a -> m a
  die        :: m a

-- We don't need any information in the errors. We could use
-- the Maybe monad transformer [1] instead (credits to Ann
-- Lillieström for that solution).
type GameT m = ErrorT Err (StateT Lives m)

type Err   = ()
type Lives = Int

-- There's no Error instance for ().
instance Error () where
  noMsg    = ()
  strMsg _ = ()

instance Monad m => GameMonad (GameT m) where
  extraLife    = modify succ
  getLives     = get
  die          = modify pred >> throwError ()
  checkPoint m = m `catchError` \e -> do
    n <- getLives
    if n > 0 then checkPoint m
             else throwError e

runGameT :: Monad m => GameT m a -> Int -> m (Maybe (a, Int))
runGameT m n = do
  (r, n) <- runStateT (runErrorT m) n
  case r of
    Left () -> return Nothing
    Right x -> return $ Just (x, n)

-- [1] http://www.haskell.org/haskellwiki/New_monads/MaybeT

