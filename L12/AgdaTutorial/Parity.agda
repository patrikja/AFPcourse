-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
module Parity where

open import Nat

-- Parity n tells us whether n is even or odd.

data Parity : Nat -> Set where
  even : (k : Nat) -> Parity (2 * k)
  odd  : (k : Nat) -> Parity (2 * k + 1)

-- Every number is either even or odd.

{-
Excerice: prove it

parity : (n : Nat) -> Parity n
parity  zero = even zero
parity (suc n)         with parity n
parity (suc .(2 * k))     | even k = {! !}
parity (suc .(2 * k + 1)) | odd  k = {! !}


half : Nat -> Nat
half n         with parity n
half .(2 * k)     | even k = k
half .(2 * k + 1) | odd  k = k
-}
