module Problem1.Deep_Instances where
import Problem1.Deep

instance Show a => Show (D a) where
  show = showD

showD :: Show a => D a -> String
showD = show . toList

instance Eq a => Eq (D a) where
 v == w   =   toList v == toList w
