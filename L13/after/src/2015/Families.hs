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
type family Add m n :: *
type instance Add Zero    n = n
type instance Add (Suc m) n = Suc (Add m n)
-- Nothing is stopping us from strange cases
type instance Add Bool Char = Float
-- type instance Add (Suc m) (Suc n) = String


{-

Exercise: implement Fin and vector indexing.

-}

data Fin n where
  -- TODO: no constructor for Fin Zero, two constructors for Fin (Suc n)
  FZ ::           Fin (Suc n)  -- Fin (Suc Zero), Fin (Suc (Suc Zero)), ...
  FS :: Fin n ->  Fin (Suc n)

zero1 :: Fin (Suc Zero)
zero1 = FZ 

zero2 :: Fin (Suc (Suc Zero))
zero2 = FZ 

one2 :: Fin (Suc (Suc Zero))
one2 = FS zero1

one :: Fin (Suc (Suc n))
one = FS FZ

two :: Fin (Suc (Suc (Suc n)))
two = FS one

-- Vector indexing:

index :: Vec a n -> Fin n -> a
-- Vec a (Suc n) -> Fin (Suc n) -> a
index (Cons x _)  FZ     = x
index (Cons _ xs) (FS n) = index xs n
index Nil _ = error "index out of bounds (absurd!)" -- Vec a Zero -> Fin Zero -> a

four :: Fin (Suc (Suc (Suc (Suc (Suc n)))))
four = FS (FS (FS (FS FZ)))
type Four = Suc (Suc (Suc (Suc Zero)))
testVec :: Vec Int Four 
testVec = Cons 1 $ Cons 7 $ Cons 3 $ Cons 8 $ Nil

-- testBad = inded testVec four


-- Compare to Promotion.hs and https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html

