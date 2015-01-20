module Problem1.Classify where
{-
----------------
-- Problem 1(a)
-- constructors:
fromList:: [a] -> Vector a
fromFun :: Length -> (Ix -> a) -> Vector a
-- combinators
(++)    :: Vector a -> Vector a -> Vector a
take    :: Ix -> Vector a -> Vector a
drop    :: Ix -> Vector a -> Vector a
splitAt :: Ix -> Vector a -> (Vector a, Vector a)
tail    :: Vector a -> Vector a
-- run functions:
head    :: Vector a -> a
index   :: Vector a -> Ix -> a
length  :: Vector a -> Length
-}
