{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}
module Promotion where
data Nat = Zero | Suc Nat
-- With the extension "DataKinds" data-definitions are "promoted":
-- So we get a new _kind_ Nat with (empty) datatypes
--   Zero :: Nat
--   Suc  :: Nat -> Nat

-- The Vec family is unchanged from Families.hs
-- Live: Copy from Families.hs and remove overlapping definitions
data Vec a n where
  Nil   :: Vec a Zero
  Cons  :: a -> Vec a n -> Vec a (Suc n)

-- Some simple functions
vhead :: Vec a (Suc n) -> a
vhead (Cons x _) = x

-- Does the definition look familiar?
vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _f Nil         = Nil
vmap  f (Cons x xs) = Cons (f x) (vmap f xs)

type Three = Suc (Suc (Suc Zero))
hello :: Num a => Vec a Three
hello = Cons 1 (Cons 2 (Cons 3 Nil))
  
(+++) :: Vec a n -> Vec a m -> Vec a (Add n m)
(+++) = error "TBD"

-- We have no predefined addition types, so we have to define it
type family Add (m :: Nat) (n :: Nat) :: Nat
type instance Add Zero    n = n
type instance Add (Suc m) n = Suc (Add m n)
-- Nothing is stopping us from strange cases
-- type instance Add Bool Char = Float
-- type instance Add (Suc m) (Suc n) = Zero



-- Mention "Promotion": https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html
