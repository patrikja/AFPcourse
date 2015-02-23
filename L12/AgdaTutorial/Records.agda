-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
module Records where
open import Basics renaming (id₃ to id)
open import Nat
open import Bool

----------------
-- A very simple record.

record Point : ⋆ where
  field
    x : Nat
    y : Nat

-- A record can be seen as a one-constructor datatype. Here:
data Point' : ⋆ where
  mkPoint : Nat -> Nat -> Point'

-- There are a few differences, though:

-- To construct a record you use the syntax
--   record { ..; x = e; .. }
origin : Point
origin = record { x = 0; y = 0 }

-- instead of
origin' : Point'
origin' = mkPoint 0 0

-- More differences are explained below - now lets move to more
-- interesting records:

⋆1 = Set1

record Functor : ⋆1 where  -- Similar to the Haskell class
  field
    F    : ⋆ -> ⋆
    fmap : {X Y : ⋆} -> (X -> Y) -> F X -> F Y
    {-
    -- could also include laws - omitted here as in Haskell
    law1 : fmap id === id
    law2 : {A B C : ⋆} ->
           (f : B -> C) -> (g : A -> B) ->
           (fmap f ∘ fmap g)  ===  fmap (f ∘ g)
    -}

{-
-- Exercise: Is Datatypes.List a Functor?
ListFunc : Functor
ListFunc = record {
             F    = {!!};
             fmap = {!!}
           }

-- Exercise: Is Families.Vec a Functor?

-}






-- Back to the differences between records and data-types

-- What's more interesting is that you get projection functions
-- for free when you declare a record. More precisely, you get a
-- module parameterised over a record, containing functions
-- corresponding to the fields. In the Point example you get:
{-
  module Point (p : Point) where
    x : Nat
    y : Nat
-}

-- So (Point.x : Point -> Nat) is the projection function for the
-- field x.
getX : Point -> Nat
getX = Point.x

-- A nifty thing with having the projection functions in a module
-- is that you can apply the module to a record value, in effect
-- opening the record.
sum : Point -> Nat
sum p = x + y
  where
   open module Pp = Point p

-- The final difference between records and datatypes is that we
-- have η-equality on records.

data _==_ {A : ⋆}(x : A) : A -> ⋆ where
  refl : x == x

η-Point : (p : Point) -> p == record { x = Point.x p; 
                                       y = Point.y p }
η-Point p = refl

----------------
-- The empty record

-- One interesting benefit of this is that we get a unit type
-- with η-equality.
record True : ⋆ where -- no fields

unit : True
unit = record{}

-- Now, since any element of True is equal to tt, metavariables
-- of type True will simply disappear. The following cute example
-- exploits this:

data False : ⋆ where

NonZero : Nat -> ⋆
NonZero zero    = False
NonZero (suc _) = True

-- We make the proof that m is non-zero implicit.

_/_ : (n m : Nat){p : NonZero m} -> Nat
(n / zero) {}
zero  / suc m = zero
suc n / suc m = div (suc n) (suc m) m
  where
    div : Nat -> Nat -> Nat -> Nat
    div  zero    zero   c = suc zero
    div  zero   (suc y) c = zero
    div (suc x)  zero   c = suc (div x c c)
    div (suc x) (suc y) c = div x y c

-- Now, as long as we're dividing by things which are obviously
-- NonZero we can completely ignore the proof.

five = 17 / 3

----------------
-- A dependent record

-- Records can be dependent, and can have parameters.
record ∃ {A : ⋆}(P : A -> ⋆) : ⋆ where
  field
    witness : A
    proof   : P witness

-- Lifted equality - suitable for the Functor laws.
_===_ : {A B : ⋆} -> (f g : A -> B) -> ⋆
f === g  =  forall x ->  f x == g x

----------------------------------------------------------------
record FunctorWithLaws : ⋆1 where  -- Similar to the Haskell class
  field
    F    : ⋆ -> ⋆
    fmap : {X Y : ⋆} -> (X -> Y) -> F X -> F Y

    -- One formulation of the laws
    law1 : {A : ⋆} ->  fmap (id {A}) === id {F A}
    law2 : {A B C : ⋆} ->
           (f : B -> C) -> (g : A -> B) ->
           (fmap f ∘ fmap g)  ===  fmap (f ∘ g)


{-
-- Exercise: Is Datatypes.List a Functor?
ListFunc : Functor
ListFunc = record {
             F    = ?;
             fmap = ?;
             law1 = ?;
             law2 = ?
           }

-- Exercise: Is Families.Vec a Functor?

-}

----------------------------------------------------------------
-- Equality is a congruence: every function preserves equality.

cong : ∀ {A B} (f : A → B) {x y}  →  x == y  →  f x == f y
cong f refl  =  refl

cong₂ : ∀ {A B C} (f : A → B → C) {x y u v} →
        x == y  →  u == v  →  f x u == f y v
cong₂ f refl refl  =  refl


module ListIsAFunctor where
  open import Datatypes hiding (Nat)

  mapIdLaw : ∀ {A} (xs : List A) ->  map id xs  ==  xs
  mapIdLaw []         = refl
  mapIdLaw (x :: xs)  = cong₂ _::_  refl  (mapIdLaw xs)

  mapCompLaw : ∀ {A B C} (f : B -> C) (g : A -> B) 
               (xs : List A) ->  
               (map f ∘ map g) xs  ==  map (f ∘ g) xs
  mapCompLaw f g []         =  refl
  mapCompLaw f g (x :: xs)  =  cong₂ _::_ refl (mapCompLaw f g xs)

  ListFunc : FunctorWithLaws
  ListFunc = record {
               F    = List;
               fmap = map;
               law1 = mapIdLaw;
               law2 = mapCompLaw
             }

  VecFunc : Nat -> Functor -- Exercise: add laws
  VecFunc n = record {
                F    = \A -> Families.Vec A n;
                fmap = Families.map
                }
    where import Families
