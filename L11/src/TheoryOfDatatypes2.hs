{-# LANGUAGE GADTs, TypeFamilies, ExistentialQuantification #-}
module TheoryOfDatatypes2 where

data Ev
data Od

type family Not a 
type instance Not Ev = Od
type instance Not Od = Ev

data EO a where
  Z   :: EO Ev
  One :: EO Od
  S   :: EO a -> EO (Not a)
  
-- fromNat :: Int -> EO ?
type Nat = Int -- to avoid imports

toNat :: EO a -> Nat
toNat Z     = 0
toNat One   = 1
toNat (S n) = 1 + toNat n

data IsEven a where
  Yes :: IsEven Ev
  No  :: IsEven Od

nott :: IsEven a -> IsEven (Not a)
nott Yes  = No
nott No   = Yes

data Q = forall t. Q (EO t) (IsEven t)
type R = Either (EO Ev) (EO Od)

fromNat :: Nat -> Q
fromNat 0  = Q  Z    Yes
fromNat 1  = Q  One  No
fromNat n  = case fromNat (n-1) of
  Q eo yn -> Q (S eo) (nott yn)

fromNatEO :: Nat -> Either (EO Ev) (EO Od)
fromNatEO n = case fromNat n of
  Q eo Yes  -> Left   eo
  Q eo No   -> Right  eo
  
q2EO :: Q -> Either (EO Ev) (EO Od)
q2EO (Q eo Yes)  = Left   eo
q2EO (Q eo No)   = Right  eo

fromNat' :: Nat -> R
fromNat' 0  = Left   Z
fromNat' 1  = Right  One
fromNat' n  = case fromNat' (n-1) of
  Left   eo  -> Right  (S eo)
  Right  eo  -> Left   (S eo)
