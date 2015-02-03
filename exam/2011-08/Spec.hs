{-
--------------------------------------------------------------
-- Problem 1a
Formulate QuickCheck properties and generators to test the correctness
of a sorting function |mysort :: [Weekday] -> [Weekday]|.
-}

import Test.QuickCheck
import Data.List((\\), sort)

ordered :: Ord a => [a] -> Bool
ordered (x:y:ys)  =  x <= y && ordered (y:ys)
ordered _         =  True -- shorter lists are always ordered

bagEq :: Eq a => [a] -> [a] -> Bool 
bagEq xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_mysort_correct :: [Weekday] -> Bool
prop_mysort_correct xs = ordered ys && bagEq xs ys
  where ys = mysort xs

instance Arbitrary Weekday where
  arbitrary = elements [Mon .. Sun]

-- Part of the problem formulation + some testing code:

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
  deriving (Eq, Ord, Show, Enum)

mysort' xs = (if length xs == 14 then reverse else id) (sort xs)
mysort xs = sort xs

main = quickCheck prop_mysort_correct

{-
-- --------------------------------------------------------------  
-- Problem 1b  

Pick any |x|.
  
Proof by induction on the list |ys| for the predicate

  P ys = length (insert x ys) === 1 + length ys

Base case |P []|:

     length (insert x [])
==  {- Def. |ins.0| -}
     length [x]
==  {- Def. |len.0| -}
     1 + length []

Case |P (y:ys)|:

Induction hypothesis |P ys| is 
  |length (insert x ys) === 1 + length ys|.

subcase |x <= y|:

     length (insert x (y:ys))
==  {- Def. |ins.1a| -}
     length (x : y : ys)
==  {- Def. |len.1| -}
     1 + length (y:ys) 

subcase |x > y|:

     length (insert x (y:ys))
==  {- Def. |ins.1b| -}
     length (y : insert x ys)
==  {- Def. |len.1| -}
     1 + length (insert x ys) 
==  {- Induction hypothesis -}
     1 + (1 + length ys)
==  {- Def. |len.1| -}
     1 + length (y:ys)

-}
