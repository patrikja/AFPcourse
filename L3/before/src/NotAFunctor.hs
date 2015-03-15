module NotAFunctor where
-- Not a functor
type NotAFunctor = Equal
newtype Equal a = Equal {runEqual :: a -> a -> Bool}

fmapEqual :: (a -> b) -> Equal a -> Equal b
fmapEqual = error "Can't be done!?"

-- Good exercise:
--   work out some examples "in between" Functor, Applicative and Monad
-- http://stackoverflow.com/questions/7220436/good-examples-of-not-a-functor-functor-applicative-monad

-- Functor, but not Applicative

-- ((,) Void) 

  

-- Applicative, but not Monad

{- ZipList
pure x                    = ZipList (repeat x)
ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
-}

-- Monad
