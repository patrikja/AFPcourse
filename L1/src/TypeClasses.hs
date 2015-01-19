module TypeClasses where

class Eq' a where            -- simplified version
  (===) :: a -> a -> Bool

  -- (/=) :: a -> a -> Bool
  -- x /= y = not (x === y)    -- default method definition

class Eq' a => Ord' a where   -- simplified version
  (<==) :: a -> a -> Bool
  (>==) :: a -> a -> Bool

instance Eq' Int where
  (===) = somePrimitiveEqualityTest
  -- ...

somePrimitiveEqualityTest :: Int -> Int -> Bool
somePrimitiveEqualityTest = (==) -- from the Haskell Prelude

instance Eq' a => Eq' [a] where 
  (===) = eqList (===)

-- 2015: skipped down to Finite
type EqT a = a -> a -> Bool

eqList :: EqT a -> EqT [a]
eqList _eq []     []      = True
eqList _eq []     (_:_)   = False
eqList _eq (_:_)  []      = False
eqList  eq (x:xs) (y:ys)  = eq x y  &&  eqList eq xs ys

class Finite a where
  domain :: [a]
  -- A finite list of all values of type a

instance Finite Bool where
  domain = [False, True]

instance (Finite a, Finite b) => Finite (a, b) where
  domain = [(x, y) | x <- domain, y <- domain]

-- Exercise: (more work)
-- instance (Finite a, Finite b) => Finite (a->b) where
--   domain = ?

-- 2015: This is the solution to a problem posed in the lecture:
instance (Finite a, Eq b) => Eq (a->b) where
  f == g   = all (\x-> f x == g x) domain

-- and some example uses:
testEqFun :: (Bool, Bool, Bool, Bool)
testEqFun = ( not == id
            , not == not
            , (\x y z -> (x&&y)||z) == (\x y z -> (x||z)&&(y||z))
            , (\x y z -> ((x&&y)||z) == ((x||z)&&(y||z))) == (\_ _ _ -> True)
            )

-- Next: Signal.hs

