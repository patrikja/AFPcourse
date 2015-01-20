import Prelude hiding ((++), head)
import qualified Data.List
import Data.List((\\))
import Test.QuickCheck

{- Task 1 (a): Imagine you should test an implementation of a function
|sort :: Ord a => [a] -> [a]|. Implement a QuickCheck property which
checks that the result is ordered and a permutation of the input.  -}

-- | Bad sort - just for testing
sort :: Ord a => [a] -> [a]
sort xs | length xs == 10  = xs
        | otherwise        = Data.List.sort xs

prop_sort_correct :: Ord a => [a] -> Bool
prop_sort_correct xs = ordered ys && bagEq xs ys
  where ys = sort xs
        
ordered :: Ord a => [a] -> Bool
ordered (x:y:ys) = x <= y && ordered (y:ys)
ordered _ = True

bagEq :: Eq a => [a] -> [a] -> Bool
bagEq xs ys = null (xs \\ ys) && null (ys \\ xs)

test = quickCheck (prop_sort_correct :: String -> Bool)

{- Task 1 (b): Explain what ``pure'' (referentially transparent) means
in a functional programming context and how it relates to equational
reasoning.
----

The value of a pure expression only depends on its free variables, not
on any state or interaction with the outside world. A pure functional
language is one where all expressions are pure. Equational reasoning
is very difficult (or impossible) for impure languages. Haskell is a
pure functional language. See also lecture 6:

"Referential transparency
* ``Equals can be substituted for equals''
* In other words: if an expression has a value in a context, we can
  replace it with any other expression that has the same value in the
  context without affecting the meaning of the program."

-}

{- Task 1 (c): Even though list concatenation is associative, that is
|lhs == (as++bs)++cs == as++(bs++cs) == rhs|, it may still be good for
performance to transform |lhs| to |rhs|. Explain why by expanding
|head lhs| and |head rhs|. You may assume that only case distinctions
(pattern matching) takes time and that |as| contains at least one
element. -}

(++) :: [a] -> [a] -> [a]
xs ++ ys = case xs of
  []       -> ys                 -- ++.1
  (x:xs')  -> x : (xs' ++ ys)    -- ++.2

head []     = error "head []"        -- head.1
head (x:xs) = x                      -- head.2

expand_lhs x xs ys zs = 
  [ head $ ((x:xs)++ys)++zs
  , -- ++.2: first step
    head $ (x:(xs++ys))++zs
  , -- ++.2: second step
    head $ x:((xs++ys)++zs)
  , -- head.2
    x
  ]

expand_rhs x xs ys zs = 
  [ head $ (x:xs)++(ys++zs)
  , -- ++.2: only step
    head $ x:(xs++(ys++zs))
  , -- head.2
    x
  ]

{- As we can see expand_lhs takes two time steps while expand_rhs only
 takes one time step. -}
