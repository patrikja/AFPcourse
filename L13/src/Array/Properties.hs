{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Array.Properties where
import Array
import Array.ShowInstances
import Array.EqInstances
import Test.QuickCheck
import Control.Monad(liftM)

prop_L1 :: (ArrayElem a, Eq a) => [a] -> Property
prop_L1 xs = not (null xs) ==>
  forAll (choose (0, length xs - 1)) $ \i ->
    fromList xs ! (Index i)  ==  xs !! i

testL1Int  = quickCheck (prop_L1 :: [Int]       -> Property)
testL1Pair = quickCheck (prop_L1 :: [(Int,Int)] -> Property)
testL1Nest = quickCheck (prop_L1 :: [Array Int] -> Property)

prop_L2 :: (ArrayElem a, Eq a) => [a] -> Bool
prop_L2 xs = toList (fromList xs) == xs

testL2Int  = quickCheck (prop_L2 :: [Int] -> Bool)
testL2Pair = quickCheck (prop_L2 :: [(Int,Int)] -> Bool)
testL2Nest = quickCheck (prop_L2 :: [Array Int] -> Bool)

sliceModel :: [a] -> (Index, Size) -> [a]
sliceModel xs (Index i, Size n) = take n (drop i xs)
toModel :: ArrayElem a => Array a -> [a]
toModel = toList

(~=) :: (ArrayElem a, Eq a) => [a] -> Array a -> Bool
model ~= impl  =  model == toModel impl

prop_L3 :: (ArrayElem a, Eq a) => [a] -> (Index, Size) -> Bool
prop_L3 xs p = sliceModel xs p ~= slice (fromList xs) p

q :: (Testable prop) => prop -> IO ()
q = quickCheck

testL3Int  = q (prop_L3 :: [Int]       -> (Index, Size) -> Bool)
testL3Pair = q (prop_L3 :: [(Int,Int)] -> (Index, Size) -> Bool)
testL3Nest = q (prop_L3 :: [Array Int] -> (Index, Size) -> Bool)

instance (Arbitrary a, ArrayElem a) => Arbitrary (Array a) where
  arbitrary = liftM fromList arbitrary

instance Arbitrary Index where -- no negative indices
  arbitrary = liftM Index $ sized (\n-> choose (0,n)) 

instance Arbitrary Size where -- no negative sizes
  arbitrary = liftM Size $ sized (\n-> choose (0,n)) 

main = sequence_ [ testL1Int, testL1Pair, testL1Nest
                 , testL2Int, testL2Pair, testL2Nest
                 , testL3Int, testL3Pair, testL3Nest
                 ]

{-
An earlier version had

*Array.Properties> testL3Nest
*** Failed! Falsifiable (after 4 tests):                  
[ArrInt [2],ArrInt [-1,-2,-1]]
(Index 1,Size 1)

The problem was that slice for nested arrays did not preserve the
invariant that the ins :: Array (Index, Size) should satisfy.

In fact, it would be more convenient if the full sum (the element
dropped by init) was also stored.

-}

test1l :: [Array Int]
test1l = [ArrInt [-2,0] ,ArrInt [],ArrInt [0]]
test1a :: Array (Array Int)
test1a = fromList test1l

-- Untested code - partial solution to the exercise.
prop_invariant :: (Eq b, Num b) => [(b, b)] -> Bool
prop_invariant iss = is == init is'
  where (is, ns) = unzip iss
        is' = scanl (+) 0 ns
