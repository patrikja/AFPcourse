module Spec where
import Test.QuickCheck
import Data.List((\\), sort)
-- Spec: use specification based development techniques
--     Spec.Test: formulate and test properties about the program
--     Spec.Prove: reason about correctness of functional programs
--     Spec.Trans: transform programs on the basis of reasoning 

-- Below is an attempt at a QuickCheck test suite for |qsort :: Ord a => [a] -> [a]|. 

qsort :: Ord a => [a] -> [a]
qsort = sort

prop_minimumBad xs         = head (qsort xs) == minimum xs

prop_orderedBad xs = ordered (qsort xs)
    where  ordered []        = True
           ordered (x:y:xs)  = x <= y && ordered (y:xs)

prop_permutationBad xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ ys)


-- Find and correct at least one bug per property in the test suite.
{-
quickCheck (prop_minimumBad :: [Int] -> Bool)
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test): 
[]
-}

prop_minimum :: [Int] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

-- Write a |main| function which tests the three properties (for lists of integers) using QuickCheck.

prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (qsort xs)
    where  ordered []        = True
           ordered [x]       = True
           ordered (x:y:xs)  = x <= y && ordered (y:xs)

prop_permutation :: [Int] -> Bool
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

main = do quickCheck prop_minimum
          quickCheck prop_ordered
          quickCheck prop_permutation


{-
  Write a sized generator (|sizedList :: Gen a -> Gen [a]|) for random
  lists. Make sure the list length is random (but bounded by the
  current size).
-}

sizedList :: Gen a -> Gen [a]
sizedList g = sized $ \n ->
              do size <- choose (0, n)
                 myVector size g

myVector :: Int -> Gen a -> Gen [a]
myVector n g = sequence (replicate n g)


----------------------------------------------------------------
-- End of exam question

-- --------------------------------------------------------------
testBad = do quickCheck (prop_minimumBad  :: [Int] -> Bool)
             quickCheck (prop_orderedBad  :: [Int] -> Bool)
             quickCheck (prop_permutationBad :: [Int] -> Bool)


----------------
-- Sanity check

sizedList' :: Gen a -> Gen (Int, [a])
sizedList' g = sized $ \n ->
              do size <- choose (0, n)
                 fmap ((,) n) $ myVector size g
