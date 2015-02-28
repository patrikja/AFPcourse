{-# LANGUAGE GADTs, KindSignatures #-}


module MaybeT.Deep where

import qualified Control.Monad.Trans as CMT
import qualified Control.Monad       as CM

-- Maybe Monad Transformer

instance Monad m => Monad (MaybeT m) where
  return = returnMT
  (>>=)  = bindMT
  fail   = failMT

returnMT :: Monad m => a ->  MaybeT m a
bindMT   :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> 
                                           MaybeT m b
failMT   :: Monad m => String -> MaybeT m a
-- liftMT   :: Monad m => m a -> MaybeT m a

-- Deep embedding: 

data MaybeT (m :: * -> *) a where
  ReturnMT :: a ->  MaybeT m a
  BindMT   :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  FailMT   :: String -> MaybeT m a

returnMT = ReturnMT
bindMT   = BindMT
failMT   = FailMT

runMaybeT:: Monad m => MaybeT m a -> m (Maybe a)
runMaybeT (ReturnMT a)   = return (Just a)
-- runMaybeT (ReturnMT a)   = return (return a)
runMaybeT (BindMT ma f)  = runBind (runMaybeT ma) (runMaybeT . f)
  -- ma :: MaybeT m a
  -- f  :: a -> MaybeT m b
  --       a -> m (Maybe b)                           
runMaybeT (FailMT err)   = return (fail err)
                           -- return Nothing 
                           -- fail err -- use the underlying monad
runBind :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> 
                                           m (Maybe b)
runBind mMa a2mMb =  mMa >>= maybe (return Nothing) a2mMb 
--  case ma of
--    Nothing  -> return Nothing
--    Just a   -> a2mMb a

-- maybe :: b -> (a -> b) -> Maybe a -> b

commute :: Maybe (m a) -> m (Maybe a)
commute Nothing   = return Nothing
commute (Just ma) = fmap Just ma