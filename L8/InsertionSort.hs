module InsertionSort where

import qualified Control.Monad   as CM
import qualified Test.QuickCheck as Q

q :: Q.Testable prop => prop -> IO ()
q = Q.quickCheck

-- The familiar insert sort function

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : xs)
  | x < y     = x : y : xs
  | otherwise = y : insert x xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)
-- or equivalently
-- > isort = foldr insert []

-- * Properties

-- | Checking that a list is ordered.
ordered :: Ord a => [a] -> Bool
ordered []           = True
ordered [_x]         = True
ordered (x : y : xs) = x <= y  &&  ordered (y : xs)

-- | 'isort' should produce ordered results.
prop_isort :: [Integer] -> Bool
prop_isort xs = ordered (isort xs)

-- | 'insert' should preserve orderedness. Bad property!  Why:
-- it's quite unlikely that a random longish list will be
-- ordered so we will only test very short lists.  'collect'ing
-- the lengths of the lists reveal this.
--
-- Fix (exercise): write a generator for ordered lists.
prop_ins :: Integer -> [Integer] -> Q.Property
prop_ins x xs = ordered xs   Q.==>
                Q.collect (length xs) (ordered (insert x xs))

-- How to test properties about how our function treats elements
-- it thinks are equal? We define a new type El

-- | 'El's are compared only on their keys when sorting.
type Key = Integer
type Value = Integer
data El = El Key Value
  deriving (Eq, Show)

instance Ord El where
  compare (El a _) (El b _) = compare a b
  -- note that compare x y == EQ   /=   x==y in general

instance Q.Arbitrary El where
  arbitrary = CM.liftM2 El Q.arbitrary Q.arbitrary

-- | Sorting twice is the same as sorting once. Not true for our
-- insertion sort!
prop_idem :: [El] -> Bool
prop_idem xs = isort (isort xs) == isort xs

-- What is wrong? (spoiler below)













----------------
-- A fixed (stable) version uses (<=) in insert
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:xs)
  | x <= y    = x : y : xs
  | otherwise = y : insert' x xs
