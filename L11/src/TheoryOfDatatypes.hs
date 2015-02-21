{-# LANGUAGE GADTs #-}
module TheoryOfDatatypes where

data Ev    -- empty datatype (standard in Haskell 2010)
data Od
data EO a where
  Z   :: EO Ev
  ES  :: EO Od -> EO Ev
  One :: EO Od
  OS  :: EO Ev -> EO Od
  
type Nat = Int -- to avoid imports

toNat :: EO a -> Nat
toNat Z     = 0
toNat One   = 1
toNat (ES o)= 1 + toNat o
toNat (OS e)= 1 + toNat e

-- | This does not work
-- fromNat :: Nat -> EO ?
fromNat 0 = Z  
-- fromNat 1 = One -- type error
fromNat n = error "fromNat: TBD"

  
