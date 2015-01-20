{-# LANGUAGE StandaloneDeriving #-}
module DSL where
import Test.QuickCheck
import Control.Monad(liftM2)

import Matrix
import qualified DSL.Deep     as D   -- Exam question 1 a
import qualified DSL.Shallow  as S   -- Exam question 1 b

----------------------------------------------------------------
-- Below is only testing code - the exam solutions are in the imported modules.

instance Arbitrary D.Shape where
  arbitrary = oneof 
    [ return D.empty                                                       
    , return D.disc                                                    
    , return D.square                                                  
    , liftM2 D.translate   arbitrary  arbitrary                               
    , liftM2 D.scale       arbitrary  arbitrary                               
    , liftM2 D.rotate      arbitrary  arbitrary                               
    , liftM2 D.union       arbitrary  arbitrary                               
    , liftM2 D.intersect   arbitrary  arbitrary                           
    , liftM2 D.difference  arbitrary  arbitrary          
    ]
    
instance Arbitrary Vec where    
  arbitrary = liftM2 vec arbitrary arbitrary

deriving instance (Show D.Shape)

instance Show Vec where
  show v = "V "++ show (vecX v) ++ " " ++ show (vecY v)

copy :: D.Shape -> S.Shape
copy (D.Empty)  =  S.empty
copy (D.Disc)   =  S.disc      
copy (D.Square) =  S.square    
copy (D.Translate   v  s)  =  S.translate     v  (copy s)
copy (D.Scale       v  s)  =  S.scale         v  (copy s)
copy (D.Rotate      a  s)  =  S.rotate        a  (copy s)
copy (D.Union       x  y)  =  S.union         (copy x)  (copy y)
copy (D.Intersect   x  y)  =  S.intersect     (copy x)  (copy y)
copy (D.Difference  x  y)  =  S.difference    (copy x)  (copy y)

prop_faithful :: D.Shape -> Point -> Bool
prop_faithful d p  =  p `D.inside` d  ==  p `S.inside` s
  where s = copy d
        
main = quickCheck prop_faithful
