{-# LANGUAGE GADTs #-}
module Datatypes where
import qualified Prelude
data Nat where         
  Zero :: Nat           
  Suc  :: Nat -> Nat

-- A simple function.
pred :: Nat -> Nat
pred Zero    = Zero
pred (Suc n) = n

-- Now let's do recursion.
(+) :: Nat -> Nat -> Nat
Zero  + m  =  m
Suc n + m  =  Suc (n + m)

infixl 6 +

-- An aside on infix operators:
-- Any name containing _ can be used as a mixfix operator.
-- The arguments simply go in place of the _. For instance:

data Bool where
  True  :: Bool
  False :: Bool

{-
if_then_else_ :: Bool -> A -> A -> A
if True  then x else y = x
if False then x else y = y
{-
-- Same result as
if_then_else_ True  x y = x
if_then_else_ False x y = y
-}
-}
----------------
-- Parameterised datatypes

{-
data List a where -- a is a parameter
  []  :: List a
  (:) :: a -> List a -> List a


{-
-- The parameters are implicit arguments to the constructors.
nil :: (a :: ⋆) -> List a
nil a = [] {a}
-}

map :: (a -> b) -> List a -> List b
map f []        = []
map f (x : xs) = f x : map f xs
-}

----------------
-- Empty datatypes

-- a very useful guy is the empty datatype.
data FALSE where   -- Not the same as False :: Bool

-- When pattern matching on an element of an empty type, something
-- interesting happens:

elim_FALSE :: FALSE -> a
elim_FALSE _ = Prelude.error "elim-FALSE: Haskell requires a RHS"

-- The pattern () is called an absurd pattern and matches elements of
-- an empty type.

----------------
-- What's next?
-- The Curry-Howard isomorphism.
--   CurryHoward.agda
