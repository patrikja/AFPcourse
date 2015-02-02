-- Monad Transformer Summary (Lecture 5, AFP 2014)
--   Patrik Jansson, Chalmers and U. of Gothenburg
-- (This is not a complete Haskell file)

----------------------------------------------------------------
-- Functors and Monads (repetition)
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

  -- Law.Id:  fmap id       ==  id
  -- Law.Cmp: fmap (f . g)  ==  fmap f . fmap g

class {- Functor m => -} Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->       m b  -> m b
  return :: a                 -> m a
  fail   :: String            -> m a

  -- Law 1.  return x >>= f   ==  f x
  -- Law 2.  m >>= return     ==  m
  -- Law 3.  (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)
  -- Law 3'. (f >=> g) >=> h  ==  f >=> (g >=> h)

liftM    :: (Monad m) => (a -> b)      -> (m a -> m b)
liftM2   :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)



----------------------------------------------------------------
-- Control.Monad.Reader
newtype    ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- newtype Reader  r   a = Reader  { runReader  :: r ->   a }
type Reader r = ReaderT r Identity

class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a



----------------------------------------------------------------
-- Control.Monad.State
newtype    StateT s m a = StateT { runStateT :: s -> m (a, s) }
-- newtype State  s   a = State  { runState  :: s ->   (a, s) }
type State s = StateT s Identity
class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

gets    :: (MonadState s m) => (s -> a) -> m a
modify  :: (MonadState s m) => (s -> s) -> m ()




----------------------------------------------------------------
-- Control.Monad.Error
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }
data Either e a = Left e | Right a -- Haskell Prelude
-- Either e is already MonadError

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a





-- CMW is not used in the Lect. 5 application, but often useful
----------------------------------------------------------------
-- Control.Monad.Writer
newtype    WriterT w m a = WriterT { runWriterT :: m (a, w) }
-- newtype Writer  w   a = Writer  { runWriter  :: (a, w) }
type Writer w = WriterT w Identity

instance (Monoid w, Monad m) => Monad (WriterT w m)
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    tell   :: w -> m ()
    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a
