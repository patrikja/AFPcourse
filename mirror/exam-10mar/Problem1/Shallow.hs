module Problem1.Shallow where
import qualified Prelude as P
import Prelude ((!!), (+), (-), (<), max, min)

----------------
-- Problem 1(b)

type Ix = P.Int
type Length = P.Int
type Vector = V
data V a = V { length :: Length, index  :: Ix -> a }

fromList xs = V (P.length xs) (xs !!)
fromFun = V
take n (V l ixf) = V (min n l) ixf
drop n (V l ixf) = V (max 0 (l-n)) (\x -> ixf (x+n))
splitAt n vec = (take n vec, drop n vec)
head v = index v 0
last vec = index vec (length vec - 1)
tail = drop 1
init vec = take (length vec - 1) vec
V l1 f1 ++ V l2 f2 = V (l1+l2) (\i->if i < l1 then f1 i else f2 (i-l1))

----------------------------------------------------------------
-- Not part of the exam question, but needed in pratice
toList :: Vector a -> [a]
toList (V n f) = P.map f [0..n-1]
