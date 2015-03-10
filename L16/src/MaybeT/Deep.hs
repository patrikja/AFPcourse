{-# LANGUAGE GADTs #-}

module MaybeT.Deep where
import qualified Control.Applicative as CA
import qualified Control.Monad.Trans as CMT
import qualified Control.Monad       as CM

-- Maybe Monad Transformer

instance Monad m => Monad (MaybeT m) where
  return = returnMT
  (>>=)  = bindMT
  fail   = failMT

instance CMT.MonadTrans MaybeT where
  lift = liftMT

returnMT :: Monad m => a ->  MaybeT m a
bindMT   :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> 
                                           MaybeT m b
failMT   :: Monad m => t -> MaybeT m a
liftMT   :: Monad m => m a -> MaybeT m a
runMaybeT:: Monad m => MaybeT m a -> m (Maybe a)







-- Deep embedding (a bit too deep)

data MaybeT m a where
  Return :: a -> MaybeT m a
  (:>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  Fail   :: MaybeT m a
  Lift   :: m a -> MaybeT m a

returnMT = Return
bindMT   = (:>>=)
failMT _ = Fail
liftMT   = Lift

runMaybeT (Return x) = return (Just x)
runMaybeT (Lift m)   = CM.liftM Just m
runMaybeT Fail       = return Nothing
runMaybeT (m :>>= f) = do
  r <- runMaybeT m
  case r of
    Nothing -> return Nothing
    Just x  -> runMaybeT (f x)

----------------

--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Monad f => Functor (MaybeT f) where
  fmap = CM.liftM
  
instance Monad f => CA.Applicative (MaybeT f) where
  pure   = return
  (<*>)  = CM.ap

-- Exercise: implement instances with weaker requirements:
--   Functor f => Functor (MaybeT f)
-- and
--   CA.Applicative f => CA.Applicative (MaybeT f)
