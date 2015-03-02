{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}
module Promotion where
data Nat = Zero | Suc Nat
-- With the extension "DataKinds" data-definitions are "promoted":
-- So we get a new _kind_ Nat with (empty) datatypes
--   Zero :: Nat
--   Suc  :: Nat -> Nat

-- The Vec family is unchanged from Families.hs
data Vec a n where
  Nil   :: Vec a Zero
  Cons  :: a -> Vec a n -> Vec a (Suc n)

-- Some simple functions
head :: Vec a (Suc n) -> a
head (Cons x _) = x    -- Providing a Nil case would be a type error

-- Does the definition look familiar?
vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ Nil          =  Nil
vmap f (Cons x xs)  =  Cons (f x) (vmap f xs)

(+++) :: Vec a n -> Vec a m -> Vec a (Add n m)
Nil         +++ ys   =   ys
(Cons x xs) +++ ys   =   Cons x (xs +++ ys)

-- The type family now has a stronger "type"
type family Add (m :: Nat) (n :: Nat) :: Nat
type instance Add Zero    m  =  m     
type instance Add (Suc n) m  =  Suc (Add n m)
-- Now we are "protected" from "kind-incorrect" cases:
-- type instance Add Bool Char = Float

{-

Exercise: implement Fin and vector indexing.

-}

















data Fin (n :: Nat) where
  FZ ::           Fin (Suc n)
  FS :: Fin n ->  Fin (Suc n)
-- no constructor for Fin Zero
  
type Fin0 = Fin Zero
type Fin1 = Fin (Suc Zero)
type Fin2 = Fin (Suc (Suc Zero))
type Fin3 = Fin (Suc (Suc (Suc Zero)))

-- Some example values
zer1 :: Fin1
zer1 = FZ

zer2, one2 :: Fin2
zer2 = FZ
one2 = FS zer1

zer3, one3, two3 :: Fin3
zer3 = FZ
one3 = FS zer2
two3 = FS one2

----------------
-- Vector indexing:

index :: Vec a n -> Fin n -> a
index (Cons x _)   (FZ)    = x
index (Cons _ xs)  (FS m)  = index xs m
index Nil          _       = error "index: an empty vector has no elements"

type Four = Suc (Suc (Suc (Suc Zero)))
testVec :: Vec Int Four 
testVec = Cons 1 $ Cons 7 $ Cons 3 $ Cons 8 $ Nil

test1 :: Int
test1 = index testVec (FS FZ)


-- Mention "Promotion": https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html
