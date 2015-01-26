module NotAFunctor where
-- Not a functor
type NotAFunctor = Equal
newtype Equal a = Equal {runEqual :: a -> a -> Bool}

fmapEqual :: (a -> b) -> Equal a -> Equal b
fmapEqual _f (Equal _op) = Equal $ \_b1 _b2 -> error "Hopeless!"

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

{- Either a -}

-- Applicative, but not Monad

{- ZipList
pure x                    = ZipList (repeat x)
ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
-}

-- Monad
