-- | Possible solution for problem 3.
module Spec where
import Test.QuickCheck
import Control.Applicative
import Data.List (sort)

data Tree a = Nil | Tree a (Tree a) (Tree a)
  deriving (Eq, Show)

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
  foldr Spec.insert Nil <$> sequence (replicate nelems gen)

-- Helper for treeUsingInsert
insert :: Ord a => a -> Tree a -> Tree a
insert x (Tree y l r)
  | x <= y    = Tree y (Spec.insert x l) r
  | x >= y    = Tree y l (Spec.insert x r)
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

main = do quickCheck (prop_ordered :: Tree Int -> Bool)
          -- the next two lines are not asked for in the exam
          testId
          testComp




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

  fmap (f . g) (Tree x l r) -- by definition of fmap
  = Tree ((f . g) x) (fmap (f . g) l) (fmap (f . g) r)
-}

----------------------------------------------------------------
-- Not asked for in the exam (just used to "check" the proofs)
instance Functor Tree where
  fmap _ Nil          = Nil
  fmap f (Tree x l r) = Tree (f x) (fmap f l) (fmap f r)

proofStepsFmapId Nil = trivial
  where trivial = []
proofStepsFmapId (Tree x l r) =
  [
    fmap id (Tree x l r)
  ,
    Tree (id x) (fmap id l) (fmap id r)
  ,
    Tree x (fmap id l) (fmap id r)
  ,
    Tree x (id l) (id r) -- by induction
  ,
    Tree x l r
  ]

allEq :: Eq a => [a] -> Bool
allEq []     = True
allEq (x:xs) = all (x==) xs

propFmapId :: Eq b => Tree b -> Bool
propFmapId t = allEq (proofStepsFmapId t)

proofStepsFmapComp :: (b -> c) -> (a -> b) ->
                      Tree a -> [Tree c]
proofStepsFmapComp f g Nil =
  [ fmap f (fmap g Nil)
  , fmap f Nil
  , Nil
  , fmap (f . g) Nil
  ]
proofStepsFmapComp f g (Tree x l r) =
  [
    fmap f (fmap g (Tree x l r))
  , fmap f (Tree (g x) (fmap g l) (fmap g r))
  , Tree (f (g x)) (fmap f (fmap g l)) (fmap f (fmap g r))
  , Tree ((f . g) x) (fmap f (fmap g l)) (fmap f (fmap g r))
  , Tree ((f . g) x) (fmap (f . g) l) (fmap (f . g) r) -- by induction
  , fmap (f . g) (Tree x l r)
  ]

propFmapComp f g t = allEq $ proofStepsFmapComp f g t

testId   = quickCheck $ (propFmapId :: Tree Int -> Bool)
testComp = quickCheck $ propFmapComp id (:"")
  -- arbitraily chosen test functions
