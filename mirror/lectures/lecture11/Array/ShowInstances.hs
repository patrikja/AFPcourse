{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Array.ShowInstances where
import Array

-- Show instances, just so we can see how the implementation works.
instance Show (Array Int) where
  showsPrec p (ArrInt a) = showParen (p > 0) $ 
    showString "ArrInt " . showsPrec 1 a

instance Show (Array Size) where
  showsPrec p (ArrSize a) = showParen (p > 0) $ 
    showString "ArrSize " . showsPrec 1 a

instance Show (Array Index) where
  showsPrec p (ArrIndex a) = showParen (p > 0) $
    showString "ArrIndex " . showsPrec 1 a

instance (Show (Array a), Show (Array b)) => Show (Array (a, b)) where
  showsPrec p (ArrPair (as, bs)) = showParen (p > 0) $
    showString "ArrPair " . showsPrec 1 (as, bs)

instance Show (Array a) => Show (Array (Array a)) where
  showsPrec p (ArrNested as segs) = showParen (p > 0) $
    showString "ArrNested " . showsPrec 1 as . showString " " 
                            . showsPrec 1 segs
