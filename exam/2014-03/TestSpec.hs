{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module TestSpec where
import Types
import Spec
import Test.QuickCheck
import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck.Arbitrary as QC

deriving instance (CoArbitrary s, Arbitrary s, Arbitrary a) => 
                  Arbitrary (State s a)


----------------------------------------------------------------
-- Testing code below - not asked for on the exam

-- Straw-man proofs in Haskell (just a list of supposedly equal expr.)
newtype AllEq a = AllEq [a] 
  deriving (Arbitrary)

instance Eq a => Testable (AllEq a) where
  property = propertyAllEq 
                          
propertyAllEq :: Eq a => AllEq a -> Property
propertyAllEq (AllEq xs) = QC.property $ QC.liftBool $ allEq xs
    
allEq []      =  True
allEq (x:xs)  =  all (x==) xs

runUnits s (Blind ma) = allEq $ map (\f -> runState (f (ma :: State Int Bool)) 
                                                    (s :: Int)) 
                                    units

unitProof'  :: ([Int], Bool) -> AllEq ([Int], Bool)
unitProof' = AllEq . unitProof

assocProof' :: ([Int],([Int],([Int],Bool))) -> AllEq ([Int],Bool)
assocProof' = AllEq . assocProof

main = do quickCheck unitProof'
          quickCheck assocProof'
          quickCheck runUnits

