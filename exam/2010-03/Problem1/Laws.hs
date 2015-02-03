module Problem1.Laws where
import qualified Problem1.Deep as D
import qualified Prelude as P
import Prelude ((==),(+),(-),($),id)
import qualified Test.QuickCheck as Q

prop_law1 xs  = D.length (D.fromList xs) == P.length xs
prop_law2 x   = D.head (D.fromList [x]) == x
prop_law3 n   = Q.forAll (Q.choose (0,n-1)) $ \i ->
                D.index (D.fromFun n id) i == i
-- ...

-- For most interesting laws an equality check is needed for vectors
--  import Problem1.Deep_Instances
prop_law4 v w = D.length (v  D.++  w) == D.length v   +   D.length w
prop_law5 v = Q.forAll (Q.choose (0,n)) $ \i ->
              D.take n v   D.++  D.drop n v   ==   v
  where n = D.length v
