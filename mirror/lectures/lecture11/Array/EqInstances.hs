{-# LANGUAGE StandaloneDeriving, 
             FlexibleInstances, FlexibleContexts #-}
module Array.EqInstances where
import Array

deriving instance Eq (Array Int)
deriving instance Eq (Array Size)
deriving instance Eq (Array Index)
deriving instance (Eq (Array a), Eq (Array b)) => 
                  Eq (Array (a,b))
deriving instance Eq (Array a) => Eq (Array (Array a))
