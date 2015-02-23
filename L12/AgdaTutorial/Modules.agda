-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
-- Let's have a closer look at the module system
module Modules where
----------------
-- Importing and opening modules

-- You can import a module defined in a different file.
import Nat  -- this is like import qualified in Haskell

-- This will bring the module into scope and allows you to
-- access its contents using qualified names.
plusTwo : Nat.Nat -> Nat.Nat
plusTwo n = Nat._+_ n 2

-- To bring everything from a module into scope you can open the
-- module. Here we do it in a nested local module.
module ALocalModule where
  open Nat

  z : Nat
  z = zero

-- Back to the top level scope - out here z is not in scope
-- anymore

-- There's also a short-hand to import and open at the same time
open import Bool  -- in Haskell this is done by default by import

_&&_ : Bool -> Bool -> Bool
x && y = if x then y else false

-- Sometimes it's nice to be able to control what is brought into
-- scope when you open a module. There are three modifiers that
-- affect this: using, hiding and renaming.

-- Nested modules are very convenient in Agda.

module DifferentWaysOfOpeningNat where

  module MoreLayersPossible where
    -- nothing but Nat
    open Nat using (Nat)

  module MoreLayersPossible2 where
    -- everything but zero
    open Nat hiding (zero)

  -- Back to DifferentWaysOfOpeningNat

  -- open everything, but zero and suc under different names
  open Nat renaming (zero to ZZ; suc to S_S)

  two : Nat
  two = S S ZZ S S  -- dist-fix notation for suc

  -- you can combine using or hiding with renaming, but not using
  -- with hiding (for obvious reasons).

-- implicit end-of-module DifferentWaysOfOpeningNat


-- To re-export something opened use the public modifier.
module A where
  open Nat public using (Nat)
-- implicit end-of-module A

N = A.Nat -- now Nat is a visible name in module A

open A -- now the type Nat is visible on the top level
open Nat hiding (Nat) -- expose the rest of module Nat as well

----------------
-- What's next?
-- The final thing on the agenda is records.
-- Move on to: Records.agda

----------------
-- Parameterised modules

-- A very useful feature is parameterised modules.

open import Families using (Vec; _::_; [])

module Sort {A : Set}(_≤_ : A -> A -> Bool) where

  insert : {n : Nat} -> A -> Vec A n -> Vec A (suc n)
  insert x [] = x :: []
  insert x (y :: ys) = if x ≤ y
                       then x :: y :: ys
                       else y :: insert x ys

  sort : {n : Nat} -> Vec A n -> Vec A n
  sort []        = []
  sort (x :: xs) = insert x (sort xs)

_≤_ : Nat -> Nat -> Bool
zero  ≤ m     = true
suc n ≤ zero  = false
suc n ≤ suc m = n ≤ m

-- When used directly, functions from parameterised modules
-- take the parameters as extra arguments.
test = Sort.sort _≤_ (6 :: 2 :: 0 :: 4 :: [])

-- But, you can also apply the entire module to its arguments.
-- Let's open the new module while we're at it.
open module SortNat = Sort _≤_

test' = sort (3 :: 2 :: 4 :: 0 :: [])

----------------
-- Local definitions

data _==_ {A : Set}(x : A) : A -> Set where
  refl : x == x

subst : {A : Set}(C : A -> Set){x y : A} -> x == y -> C x -> C y
subst C refl cx = cx

cong : {A B : Set}(f : A -> B){x y : A} -> x == y -> f x == f y
cong f refl = refl

lem₁ : (n : Nat) -> n + 0 == n
lem₁ zero    = refl
lem₁ (suc n) = cong suc (lem₁ n)

{-
lem₂ : (n m : Nat) -> n + suc m == suc n + m
lem₂ n m = ?  -- Excercise
-}

