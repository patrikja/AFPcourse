module SubsetMoreExplicit where
open import Nat
open import Datatypes

data _⊆_ {A : Set} : List A -> List A -> Set where
  stop : []  ⊆  []
  keep : (x : A) -> {xs ys : List A} ->  
         xs ⊆ ys  ->  x :: xs  ⊆  x :: ys
  drop : forall {xs} (y : A) {ys} ->  
         xs ⊆ ys  ->       xs  ⊆  y :: ys

test :  1 :: []  ⊆  1 :: 2 :: []
test = keep 1 (drop 2 stop)