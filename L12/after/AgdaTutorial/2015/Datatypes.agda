-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)

-- This is where the fun begins.  Unleashing datatypes, pattern
-- matching and recursion.
module Datatypes where
⋆ = Set   -- Could be import-ed from Basics
----------------------------------------------------------------
-- Simple datatypes.

-- Let's define natural numbers.

data Nat : ⋆ where     -- kind / type annotation required
  zero : Nat           -- Agda always uses "GADT syntax"
  suc  : Nat -> Nat

-- A simple function defined by pattern mathcing.
pred : Nat -> Nat
pred zero    = zero
pred (suc n) = n

-- Now let's do recursion.
_+_ : Nat -> Nat -> Nat
zero  + y = y
suc x + y = suc (x + y)
--  C-c C-c  case-split

infixl 60 _+_

-- An aside on infix operators: Any name containing _ can be used as a
-- mixfix operator.  The arguments simply go in place of the _. For
-- instance: if _+_ would be called _plus_ the word plus could be used
-- infix instead of the operator +.

data Bool : ⋆ where
  true  : Bool
  false : Bool

if_then_else_ : {A : ⋆} -> Bool -> A -> A -> A
if true  then t else e = t
if false then t else e = e

----------------
-- Parameterised datatypes

data List (A : ⋆) : ⋆ where -- A is a parameter
  []   : List A
  _::_ : A -> List A -> List A

-- The parameters are implicit arguments to the constructors.
-- To illustrate that we define an explicit version: nil.
-- [] : {A : ⋆} -> List A
nil : (A : ⋆) -> List A
nil A = [] {A}

map : ∀ {A B : ⋆} -> (A -> B) -> List A -> List B
map f []        = []
map f (x :: xs) = f x :: map f xs
-- Emacs: C-c C-c    case split
--        C-c C-a    "auto tactic" -- beware!
----------------
-- Empty datatypes

-- A very useful(?) type is the empty datatype.
data FALSE : ⋆ where   -- Not the same as false : Bool : ⋆

-- When pattern matching on an element of an empty type, something
-- interesting happens:

elim-FALSE : ∀ {A : ⋆} -> FALSE -> A
elim-FALSE () 

-- The pattern () is called an absurd pattern and "matches"
-- the non-existent elements of an empty type.

----------------
-- What's next?
-- The Curry-Howard isomorphism.
--   CurryHoward.agda








infixr 40 _::_

