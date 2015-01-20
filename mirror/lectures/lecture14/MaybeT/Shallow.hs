{-# LANGUAGE GADTs #-}

module MaybeT.Shallow where

import qualified Control.Monad.Trans as CMT
import qualified Control.Monad       as CM

-- Monad Transformers

-- Shallow embedding (nicer)

{- A general pattern for defining a monad transformer is

    newtype SomeT pars m a = SomeT
      { runSomeT :: args -> m (something wrapping a) }

That is, a computation in the transformed monad is a computation
in the underlying monad with a more interesting result type,
possibly with some additional arguments.

   From the libraries:
     --        args        "something wrapping a"
     StateT:   s ->      m (a, s)
     ReaderT:  e ->      m a
     WriterT:            m (a, w)
     ErrorT:             m (Either e a)
   -- Reader, Writer and State all at once:
     RWST:     e -> s -> m (a, s, w)
-}
-- For our case: no args and "Maybe" wrapping a
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = returnMT
  (>>=)  = bindMT
  fail   = failMT

returnMT :: Monad m => a ->  MaybeT m  a
returnMT x = MaybeT $ return (Just x)

bindMT :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT m f = MaybeT $ do -- in the Monad m
  r <- runMaybeT m
  case r of
    Nothing -> return Nothing
    Just x  -> runMaybeT (f x)

failMT :: Monad m => t -> MaybeT m a
failMT _ = MaybeT (return Nothing)

instance CMT.MonadTrans MaybeT where
  lift = liftMT

liftMT :: Monad m => m a -> MaybeT m a
liftMT m = MaybeT (CM.liftM Just m)
