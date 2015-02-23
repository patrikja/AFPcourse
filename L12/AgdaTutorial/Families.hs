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
head (Cons x _) = x    -- Providing a Nil case would be a type error


-- Does the definition look familiar?
vmap :: (a -> b) -> Vec a n -> Vec b n
vmap f Nil          =  Nil
vmap f (Cons x xs)  =  Cons (f x) (vmap f xs)

(+++) :: Vec a n -> Vec a m -> Vec a (Add n m)
Nil         +++ ys   =   ys
(Cons x xs) +++ ys   =   Cons x (xs +++ ys)

-- We have no predefined addition types, so we have to define it
type family Add m n :: *
type instance Add Zero    m  =  m     
type instance Add (Suc n) m  =  Suc (Add n m)
  
-- The identity type.
data Equal a b where
  Refl :: Equal a a

{-

Exercise: implement Fin and vector indexing.

-}

















data Fin n where
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
one2 = FS fz1

zer3, one3, two3 :: Fin3
zer3 = FZ
one3 = FS zer2
two3 = FS one2

