module NotAFunctor where
-- Not a functor
type NotAFunctor = Equal
newtype Equal a = Equal {runEqual :: a -> a -> Bool}
  --                                 Time -> a
cofmapEqual :: (b -> a) -> (Equal a -> Equal b)
cofmapEqual finv (Equal op) = Equal (\b1 b2 -> op (finv b1) (finv b2) )
  --  f :: a -> b
  -- op :: a -> a -> Bool
  -- a1, a2 :: a
  -- finv :: b -> a                         





















cofmapEqual :: (a -> b) -> Equal b -> Equal a
cofmapEqual f (Equal op) = Equal (\a1 a2 -> op (f a1) (f a2))

{-
fmap   :: Functor f   => (a -> b) -> f a -> f b
cofmap :: Cofunctor f => (a -> b) -> f b -> f a
-}

-- Good exercise:
--   work out some examples "in between" Functor, Applicative and Monad
-- http://stackoverflow.com/questions/7220436/good-examples-of-not-a-functor-functor-applicative-monad

-- Functor, but not Applicative

instance Functor ((,) r) where
  fmap f (x,y) = (x, f y)

-- This is not in general an Applicative for all r. 

  

-- Applicative, but not Monad

{- ZipList
pure x                    = ZipList (repeat x)
ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
-}

-- Monad
