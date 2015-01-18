module TypeClasses where

class Eq' a where            -- simplified version
  (==) :: a -> a -> Bool

  -- (/=) :: a -> a -> Bool
  -- x /= y = not (x TypeClasses.== y)    -- default method definition

class Eq' a => Ord' a where   -- simplified version
  (<=) :: a -> a -> Bool
  (>=) :: a -> a -> Bool

instance Eq' Int where
  (==) = somePrimitiveEqualityTest
  -- ...

instance Eq' a => Eq' [a] where 
  (==) = eqList (TypeClasses.==)

type EqT a = a -> a -> Bool

eqList :: EqT a -> EqT [a]
eqList _eq []     []      = True
eqList _eq []     (_:_)   = False
eqList _eq (_:_)  []      = False
eqList  eq (x:xs) (y:ys)  = eq x y  &&  eqList eq xs ys




somePrimitiveEqualityTest :: Int -> Int -> Bool
somePrimitiveEqualityTest = (Prelude.==) -- from the Haskell Prelude
