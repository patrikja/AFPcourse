-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
module Filter where
open import Nat
open import Datatypes

filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter p [] = []
filter p (x :: xs) with p x
filter p (x :: xs)    | true   = x :: filter p xs
filter p (x :: xs)    | false  =      filter p xs

{- There is no "case" statement in Agda, but there is a "with"
instead.  -}

-- Formulating and proving properties
-- Let's try to prove that filter yields a subsequence
-- First we need to define what that means - here we will define
-- it inductively. This means we define a datatype containing
-- concrete evidence about why one sequence is included in
-- another.

infix 20 _⊆_

data _⊆_ {A : Set} : List A -> List A -> Set where
  stop  :  []  ⊆  []
  keep  :  ∀ {x xs ys} ->  xs ⊆ ys  ->  x :: xs  ⊆  x :: ys
  drop  :  ∀ {xs y ys} ->  xs ⊆ ys  ->       xs  ⊆  y :: ys

{-
-- Warm-up exercise:
test :  1 :: []  ⊆  1 :: 2 :: []
test = {!!}
-}

{-
-- Exercise: prove SubsetTheorem
SubsetTheorem = {A : Set} -> (p : A -> Bool) -> (xs : List A) ->
                filter p xs ⊆ xs
subset : SubsetTheorem
subset = ?
-}

{-
C-c C-c   "make case"
C-c C-r   "refine"
-}



-- Next: Modules.agda (if time permits)









{- 
test :  1 :: []  ⊆  1 :: 2 :: []
test = keep (drop stop)  -- Warm-up solution
-}


{-
subset p []        = stop
subset p (x :: xs) with p x
... | true  = keep (subset p xs)
... | false = drop (subset p xs)
-}

{-

The "..." is part of the Agda syntax and allows the constant part
of the pattern to be elided.

subset p (x :: xs) with p x
subset p (x :: xs) | true  = keep (subset p xs)
subset p (x :: xs) | false = drop (subset p xs)

-}



