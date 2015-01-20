module Problem1.Generator where
import qualified Prelude as P
import Prelude
import qualified Problem1.Shallow as V
import qualified Test.QuickCheck as Q
import qualified Control.Monad as CM
import qualified Problem1.Deep_Instances ()
import Problem1.Deep
import Problem1.Laws

instance Q.Arbitrary a => Q.Arbitrary (D a) where
  arbitrary = Q.sized arbitraryD

arbitraryD :: Q.Arbitrary a => Int -> Q.Gen (D a)
arbitraryD n | n <= 1     =  Q.oneof [ CM.liftM single1 Q.arbitrary
                                     , CM.liftM single2 Q.arbitrary ]
             | otherwise  =  do left <- Q.choose (1,n-1)
                                CM.liftM2 (:++) (arbitraryD left) (arbitraryD (n-left))

single1 :: a -> D a
single1 a = FromList [a]

single2 :: a -> D a
single2 a = FromV (V.V 1 (P.const a))

----------------------------------------------------------------
-- Not part of the exam question: run some tests
q :: Q.Testable p => p -> IO ()
q = Q.quickCheck

main = do q (prop_law1 :: [()] -> Bool)
          q (prop_law2 :: Int -> Bool)
          q (prop_law3 :: Int -> Q.Property)
          q (prop_law4 :: Vector () -> Vector () -> Bool)
          q (prop_law5 :: Vector Int -> Q.Property)
