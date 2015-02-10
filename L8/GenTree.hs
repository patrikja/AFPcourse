module GenTree where
import Test.QuickCheck -- (frequency)
import Control.Monad (liftM, liftM2)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

arbBad :: Arbitrary a => Gen (Tree a)
arbBad = frequency [ (1, liftM   Leaf   arbitrary)
                   , (2, liftM2  Node   arbBad arbBad)
                   ]

{- Termination?

let p be the probability that Leaf is chosen
let t be the probability that the tree is finite

We can set up an equation for t by noting that a Leaf is finite, and
that a Node is finite iff both subtrees are finite. If these are
generated independently we get.

  t == p + (1-p) * t * t

(We can directly see that t == 1 is a solution.)
The other solution can be found by factoring / rewriting

  t == p + (1-p)*t*t
= -- all on one side
  (1-p)*t*t                - t + p == 0
= -- try to factor out (t-1) from the t*t term
  (1-p)*t*(t-1) + (1-p)*t  - t + p == 0
=  -- simplify the rest
  (1-p)*t*(t-1) - p*t + p == 0
=  -- factor out (t-1) also in the rest
  (1-p)*t*(t-1) - p*(t-1) == 0
=  -- distributivity
  ((1-p)*t - p) * (t-1) == 0

Thus the two "raw" solutions are
  t1 == 1
  t2 == p/(1-p)

(A "raw" solution is only a real solution when it is 0<=t<=1.)

For the example above, where p == 1/3 we get t == 1/2.
Thus, the generator terminates only half of the time!

This generator is bad in two ways - if generates Leaf 1/3 of the time
(far too often) and it generates a finite non-Leaf tree only in 1/6 of
the cases!.

-}
