-- | Possible solution for problem 3.
module Spec where
import Test.QuickCheck
import Control.Applicative
import Data.List (sort)

data Tree a = Nil | Tree a (Tree a) (Tree a)
  deriving Show

-- Problem a: BST generator
-- Possible solution: generate sorted random list, split it at arbitrary points
treeUsingList :: Ord a => Gen a -> Gen (Tree a)
treeUsingList gen = sized $ \n -> do
    nelems <- choose (0, n)
    sequence (replicate nelems gen) >>= go . sort
  where
    go [] = return Nil
    go xs = do
      pivot <- choose (0, length xs-1)
      let (l, x : r) = splitAt pivot xs
      Tree x <$> go l <*> go r

-- Another possible solution: generate random list, insert elements into tree
treeUsingInsert :: Ord a => Gen a -> Gen (Tree a)
treeUsingInsert gen = sized $ \n -> do
  nelems <- choose (0, n)
  foldr Tree.insert Nil <$> sequence (replicate nelems gen)

-- Helper for treeUsingInsert
insert :: Ord a => a -> Tree a -> Tree a
insert x (Tree y l r)
  | x <= y    = Tree y (Tree.insert x l) r
  | x >= y    = Tree y l (Tree.insert x r)
  | otherwise = error "Impossible!"
insert x _    = Tree x Nil Nil




-- Problem b: ordered property
prop_ordered :: Ord a => Tree a -> Bool
prop_ordered = isOrdered . inorder
  where
    isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)
    isOrdered _        = True

-- Helper for ordered property
inorder :: Tree a -> [a]
inorder Nil          = []
inorder (Tree x l r) = inorder l ++ x : inorder r




-- Problem c: Arbitrary instance and main function
instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = treeUsingList arbitrary

depth :: Tree a -> Int
depth Nil          = 0
depth (Tree x l r) = 1 + max (depth l) (depth r)

main = quickCheck (prop_ordered :: Tree Int -> Bool)




-- Problem d: prove the functor laws
{-
  Law 1: fmap id = id

  fmap id Nil
  = Nil

  fmap id (Tree x l r)
  = Tree (id x) (fmap id l) (fmap id r)
  = Tree x (fmap id l) (fmap id r)
  = Tree x (id l) (id r) -- by induction
  = Tree x l r

  ---

  id Nil
  = Nil

  id (Tree x l r)
  = Tree x l r
-}

{-
  Law 2: fmap f . fmap g == fmap (f . g)

  fmap f (fmap g Nil)
  = fmap f Nil
  = Nil

  fmap f (fmap g (Tree x l r))
  = fmap f (Tree (g x) (fmap g l) (fmap g r))
  = Tree (f (g x)) (fmap f (fmap g l)) (fmap f (fmap g r))
  = Tree ((f . g) x) (fmap f (fmap g l)) (fmap f (fmap g r))
  = Tree ((f . g) x) (fmap (f . g) l) (fmap (f . g) r) -- by induction

  ---

  fmap (f . g) Nil
  = fmap WHATEVER Nil -- by definition of fmap
  = Nil

  fmap (f . g) (Tree x l r)
  = fmap (\x -> f (g x)) (Tree x l r)
  = Tree ((f . g) x) (fmap (f . g) l) (fmap (f . g) r)
-}
