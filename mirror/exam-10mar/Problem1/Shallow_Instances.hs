module Problem1.Shallow_Instances where
import Problem1.Shallow

instance Show a => Show (V a) where
  show = show . toList

