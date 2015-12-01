module Correctness where
import Data.IORef
import InsertionSort

impureTest = do
  x  <-  newIORef 0
  let f n = do modifyIORef x (+n); readIORef x
  r  <-  f 1
  s  <-  f 2
  return (r + s)

-- Equational reasoning example

{-
length :: [a] -> Int
length []      =  0
length (x:xs)  =  1 + length xs

isort :: Ord a => [a] -> [a]
isort []      =  []
isort (x:xs)  =  insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x []      =  [x]
insert x (y:ys)
  | x <= y       =  x : y : ys
  | otherwise    =  y : insert x ys
-}

{-
* Theorem: Sorting preserves length
forall ((xs :: [a])) . length (isort xs) === length xs

* Lemma:
forall ((x :: a)) (ys :: [a]) . length (insert x ys) === 1 + length ys
-}

-- Poor man's "equality proofs" - just to enable type checking and testing.
type EqProof = []

lemma_case_nil :: Ord a => a -> EqProof Int
lemma_case_nil x =
  [
       length (insert x [])
  ,    {- Definition of |insert| -}
       length [x]
  ,    {- Definition of |length| -}
       1 + length []
  ]

lemma_case_cons_LTE :: Ord a => a -> a -> [a] -> EqProof Int
lemma_case_cons_LTE x y ys =
  [
       length (insert x (y:ys))
  ,    {- Definition of |insert| -}
       length (x : y : ys)
  ,    {- Definition of |length| -}
       1 + length (y:ys)
  ]

lemma_case_cons_GT :: Ord a => a -> a -> [a] -> EqProof Int
lemma_case_cons_GT x y ys =
  [
       length (insert x (y:ys))
  ,    {- Definition of |insert| -}
       length (y : insert x ys)
  ,    {- Definition of |length| -}
       1 + length (insert x ys)
  ,    {- Induction hypothesis -}
       1 + (1 + length ys)
  ,    {- Definition of |length| -}
       1 + length (y:ys)
  ]

theorem_nil :: EqProof Int
theorem_nil =
  [
       length (isort ([] :: [Int]))
  ,    {- Definition of |isort| -}
       length []
  ]

theorem_cons :: Ord a => a -> [a] -> EqProof Int
theorem_cons x xs =
  [
       length (isort (x:xs))
  ,    {- Definition of |isort| -}
       length (insert x (isort xs))
  ,    {- Lemma -}
       1 + length (isort xs)
  ,    {- Induction hypothesis -}
       1 + length xs
  ,    {- Definition of |length| -}
       length (x:xs)
  ]
