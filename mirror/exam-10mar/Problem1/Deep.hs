{-# LANGUAGE GADTs #-}
module Problem1.Deep where
import qualified Prelude as P
import Prelude(Int,(+),(-),max,min,(<),(<=),(==),(&&),Bool,Show,String, (!!), otherwise, div)
import qualified Problem1.Shallow as V
import qualified Problem1.Shallow_Instances ()
----------------
-- Problem 1(c)

type Ix = P.Int
type Length = P.Int
type Vector = D

data D a where
  FromList :: [a] -> D a
  FromV    :: V.V a -> D a
  (:++)    :: D a -> D a -> D a

-- primitive
(++)      ::  D a -> D a -> D a
(++) = (:++)

length    ::  Vector a -> Length
length (xs :++ ys)    =  length xs + length ys
length (FromList xs)  =  P.length xs
length (FromV v)      =  V.length v

drop      ::  Ix -> Vector a -> Vector a
drop i (xs :++ ys) | i <= l     =  drop i xs :++ ys
                   | otherwise  =  drop (i-l) ys
  where l = length xs
drop i (FromList xs)  =  FromList (P.drop i xs)
drop i (FromV v)      =  FromV (V.drop i v)

take      ::  Ix -> Vector a -> Vector a
take i (xs :++ ys) | i <= l     =  take i xs
                   | otherwise  =  xs :++ take (i-l) ys
  where l = length xs
take i (FromList xs)            =  FromList (P.take i xs)
take i (FromV v)                =  FromV (V.take i v)

-- Derived operations:

splitAt   ::  Ix -> Vector a -> (Vector a, Vector a)
splitAt i v = (take i v, drop i v)

-- --------------------------------------------------------------
-- For completeness (not asked for in exam question):

fromFun   ::  Length -> (Ix -> a) -> Vector a
fromFun l f = FromV (V.V l f)

fromList  ::  [a] -> Vector a
fromList = FromList

index     ::  Vector a -> Ix -> a
index (xs :++ ys) i | i<l        =  index xs i
                    | otherwise  =  index ys (i-l)
  where l = length xs
index (FromList xs) i            = xs P.!! i
index (FromV v) i                = V.index v i

head      ::  Vector a -> a
head xs = index xs 0

tail      ::  Vector a -> Vector a
tail = drop 1

----------------------------------------------------------------
-- Not part of the exam question, but needed in pratice
toList :: Vector a -> [a]
toList (FromList xs)  =  xs
toList (FromV v)      =  V.toList v
toList (v :++ w)      =  toList v P.++ toList w


