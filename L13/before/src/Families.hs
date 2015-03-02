{-# LANGUAGE TypeFamilies, GADTs #-}
module Families where
type Nat = Int

-- The "Silly" function can be simulated using type families
type family Silly n :: *
type instance Silly Zero           =  Bool
type instance Silly (Suc Zero)     =  Nat
type instance Silly (Suc (Suc n))  =  (Silly n  ,  Silly (Suc n))
-- But note that the argument n is a type, not a natural number.
-- Thus we cannot pattern match on it to define "silly".

-- The Vec family can be "simulated" using type level Nat
data Zero
data Suc n

data Vec a n where
  Nil   :: Vec a Zero
  Cons  :: a -> Vec a n -> Vec a (Suc n)

-- Some simple functions
head :: Vec a (Suc n) -> a
head = error "TBD"

-- Does the definition look familiar?
vmap :: (a -> b) -> Vec a n -> Vec b n
vmap = error "TBD"

(+++) :: Vec a n -> Vec a m -> Vec a (Add n m)
(+++) = error "TBD"

-- We have no predefined addition types, so we have to define it
type family Add m n :: *
-- to be completed

-- Nothing is stopping us from strange cases

{-

Exercise: implement Fin and vector indexing.

-}

data Fin n -- TODO: no constructor for Fin Zero, two constructors for Fin (Suc n)
  
-- Vector indexing:

index :: Vec a n -> Fin n -> a
index = error "TBD"

type Four = Suc (Suc (Suc (Suc Zero)))
testVec :: Vec Int Four 
testVec = Cons 1 $ Cons 7 $ Cons 3 $ Cons 8 $ Nil



-- Compare to Promotion.hs and https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html

