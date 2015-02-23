-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)

-- Now we're getting somewhere! Really dependent types.

module Families where -- generalization of GADTs in Haskell
-- You can import modules defined in other files.
-- More details later...
open import Basics using (⋆)
open import Nat
open import Bool
open import CurryHoward using (_∧_; _,_)

-- First an example of computing a family of datatypes
Silly : Nat -> ⋆
Silly 0             = Bool
Silly 1             = Nat
Silly (suc (suc n)) = Silly n  ∧  Silly (suc n)

-- ... and an example of a dependently typed function
silly : (n : Nat) -> Silly n
silly 0             = false
silly 1             = 1738
silly (suc (suc n)) = (silly n , silly (suc n))

test : Silly 3
test = (5 , (true , 8))
test2 : Silly 4
test2 = ((false , 17) , test)

-- Nat ∧ (Silly 0 ∧ Silly 1) = Nat ∧ (Bool ∧ Nat)
-- test : (Nat, (Bool, Nat))

{- no non-const functions from ⋆ to Nat
fun : ⋆ -> Nat
fun i = 1   -- "Int" is just a (bad) variable name
-}






-- Now on to inductive families of datatypes - similar to GADTs in
-- Haskell.

data Vec (A : ⋆) : Nat -> ⋆ where
  []   : Vec A zero
  _::_ : ∀ {n : Nat} -> A -> Vec A n -> Vec A (suc n)

-- In Haskell, the same can be simulated with "type level Nat"

infixr 40 _::_

-- Some simple functions
head : {A : ⋆}{n : Nat} -> Vec A (suc n) -> A
head (x :: _) = x  -- no need for a [] case, so it cannot fail

-- Does the definition look familiar?
map : {A B : ⋆}{n : Nat} -> (A -> B) -> Vec A n -> Vec B n
map f []        = []
map f (x :: xs) = f x :: map f xs

infixr 40 _++_

_++_ : {A : ⋆}{n m : Nat} -> Vec A n -> Vec A m -> Vec A (n + m)
[]        ++ ys   =   ys
(x :: xs) ++ ys   =   x :: (xs ++ ys)

-- Why does this type check? Let's walk through it slowly.
-- When pattern matching on the first vector, n is instantiated.

-- What happens if we make the lengths explicit?

cat : (A : ⋆)(n m : Nat) -> Vec A n -> Vec A m -> Vec A (n + m)
cat A .0       m []               ys = ys
cat A .(suc n) m (_::_ {n} x xs)  ys = x :: cat A n m xs ys

-- Patterns which get instantiated by pattern matching on other
-- stuff get tagged by a dot. If you erase all the dotted things
-- you get a well-formed linear first-order pattern.

-- Let's do some other interesting families.

-- The identity type.
data _==_ {A : ⋆} : A -> A -> ⋆ where
  refl : (x : A)  ->  x == x

trans : {A : ⋆} -> {x y z : A} -> (x == y) -> (y == z) -> (x == z)
trans {A} {.z} {.z} {z} (refl .z) (refl .z) = refl z


subst : {A : ⋆} -> (C : A -> ⋆) ->  (x y : A) ->
        x == y   ->   (C x -> C y)
subst C .x .x (refl x) cx = cx

{- Finite sets
Type             Corresponding set
Fin zero         {}
Fin (suc zero)   {fzero}
Fin 2            {fzero, fsuc fzero}
...              ...
-}
data Fin : Nat -> ⋆ where
  fzero : {n : Nat}  ->             Fin (suc n)
  fsuc  : {n : Nat}  ->  Fin n  ->  Fin (suc n)

-- Note that there are two constructors for creating a number in
-- Fin (suc n) but no constructor for Fin zero.

-- Indexing into vectors
_!_ : {A : ⋆}{n : Nat} ->  Vec A n ->  (Fin n -> A)
[]        ! ()
(x :: xs) ! fzero  = x
(x :: xs) ! fsuc i = xs ! i

----------------
-- What's next?
-- Actually, inductive families are sufficiently fun that
-- you'll never get bored, but there's even more fun to be had.

-- Move on to: Filter.agda










{-
See FamiliesExtra.agda
postulate  plusComm : ∀ (n m : Nat) ->  (n + m) == (m + n)
postulate  coerceVec : ∀ {A : ⋆} (n m : Nat) ->  
                      Vec A (n + m) -> Vec A (m + n)
cat' : {A : ⋆}(n m : Nat) -> Vec A n -> Vec A m -> Vec A (m + n)
cat' n m xs ys = coerceVec n m (cat n m xs ys)
-}
