{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, 
             UndecidableInstances #-}
module Types where
import Control.Monad.Writer
-- Exam question:
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
 
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ runMaybeT x >>= 
               maybe (return Nothing) 
                     (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing
 
instance (Monad m) => Monad (MaybeT m) where
  return  =  returnMT
  (>>=)   =  bindMT
  fail    =  failMT

problem :: MonadWriter [String] m => m Int
problem = do
  tell ["I fail"]
  fail "oops"
  return 1738
  error "hej" -- not part of the exam question

type A = WriterT [String] Maybe
type B = MaybeT (Writer [String])

a :: A Int
a = problem

b :: B Int
b = problem


-- What do |runWriterT a| and |runWriter (runMaybeT b)| evaluate to?  
-- Explain.

testa = runWriterT a
testb = runWriter (runMaybeT b)

-- For both tests, the call to return has no effect, because the call
-- to fail makes the computation fail before it gets to return.

{- 
A a = WriterT [String] Maybe a ~= Maybe (a, [String])

With this type, fail will give Nothing, and only is non-failing cases
will there be a [String] to present. (No good for logging!)

testa == Nothing
-}

{- 
type B a = MaybeT (Writer [String]) a ~= Writer [String] (Maybe a) 
        ~= (Maybe a, [String])

With this type, there will always be a String as the second component
of the pair, even when fail gives a Nothing as the first component.

testb == (Nothing,["I fail"])

-}


instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell    =  tellMT
  listen  =  listenMT
  pass    =  error "pass is not part of the exam question"

tellMT   :: MonadWriter w m => w -> MaybeT m ()
tellMT w = MaybeT $ tellMT1 w

-- I'm adding type annotations just to clarify a bit.
tellMT1  :: MonadWriter w m => w -> m (Maybe ())
tellMT1 w = tell w >> return (Just ())

listenMT :: MonadWriter w m => MaybeT m a -> MaybeT m (a, w)
listenMT (MaybeT mMa) = MaybeT $ listenMT1 mMa

listenMT1 :: MonadWriter w m => m (Maybe a) -> m (Maybe (a, w))
listenMT1 mma = do (ma, w) <- listen mma
                   return $ case ma of
                     Nothing -> Nothing
                     Just a  -> Just (a, w)

----------------------------------------------------------------
main = do 
  print $ testa == Nothing
  print $ testb == (Nothing,["I fail"])
