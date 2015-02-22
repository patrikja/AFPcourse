{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Array where

{- A type family is like a function on the type level where you can pattern
   match on the type arguments. An important difference is that it's /open/
   which means that you can add more equations to the function at any time.

   In this example we will implement a type family of efficient array
   implementations. 
-}

-- We're cheating a little here to make things easy :-)
type SuperEfficientByteArray = [Int]

-- The type signature for our family.
data family Array a

-- The definition of our family. The most interesting case is the nested arrays
-- which we implement as a flat array together with an array of offsets and
-- lengths of the subarrays.
newtype instance Array Int       = ArrInt SuperEfficientByteArray
data    instance Array (a, b)    = ArrPair (Array a) (Array b)
data    instance Array (Array a) = ArrNested (Array a) (Array (Index, Size))

type Index = Int
type Size  = Int

-- | A class of operations on the array family. We'll make one instance per
--   clause in the definition of the family.
class ArrayElem a where
  (!)      :: Array a -> Index -> a
  -- | @slice a (i,n)@ is the slice of @a@ starting at index @i@ with @n@
  --   elements.
  slice    :: Array a -> (Index, Size) -> Array a
  size     :: Array a -> Size
  fromList :: [a] -> Array a

-- | Converting an array to a list.
toList :: ArrayElem a => Array a -> [a]
toList a = [ a ! i | i <- [0..size a - 1] ]

-- Int arrays. Since we're cheating with the implementation of
-- int arrays the implementations of the functions are easy.
instance ArrayElem Int where
  ArrInt a ! i = a !! i

  slice (ArrInt a) (i, n) =
    ArrInt $ take n $ drop i a

  size (ArrInt a) = length a

  fromList xs = ArrInt xs

-- Arrays of pairs are pairs of arrays. Mostly just lifting the operations to
-- work on pairs.
instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where

  ArrPair as bs ! i = (as ! i, bs ! i)

  slice (ArrPair as bs) (i, j) =
    ArrPair (slice as (i, j)) (slice bs (i, j))

  size (ArrPair as _) = size as

  fromList xs = ArrPair (fromList as) (fromList bs)
    where (as, bs) = unzip xs

-- Nested arrays. Here we have to work a little bit.
instance ArrayElem a => ArrayElem (Array a) where

  -- Indexing is just slicing the flat array.
  ArrNested as segs ! i = slice as (segs ! i)

  -- Slicing is slicing the flat array and the segment array,
  -- but we have to do some work to figure out how to slice the
  -- flat array.
  slice (ArrNested as segs) (i, n) =
    ArrNested (slice as (j, m))
              (slice segs (i, n))
    where
      (j, _) = segs ! i
      m      = sum [ snd (segs ! k) | k <- take n [i..] ]

  size (ArrNested _ segs) = size segs

  -- Creating the flat array isn't very nicely done. We shouldn't
  -- have to convert our arrays back to lists to do it.
  -- Solution (exercise): add a concatenation operation on arrays.
  fromList xss = ArrNested (fromList $ concat $ map toList xss) -- Bad!
                           (fromList $ tail $ scanl seg (0,0) xss)
    where seg (i, n) a = (i + n, size a)

-- Show instances, just so we can see how the implementation works.
instance Show (Array Int) where
  show (ArrInt a) = show a

instance (Show (Array a), Show (Array b)) => Show (Array (a, b)) where
  showsPrec p (ArrPair as bs) = showParen (p > 0) $
    showString "ArrPair " . showsPrec 1 as . showString " " . showsPrec 1 bs

instance Show (Array a) => Show (Array (Array a)) where
  showsPrec p (ArrNested as segs) = showParen (p > 0) $
    showString "ArrNested " . showsPrec 1 as . showString " " . showsPrec 1 segs

-- An example array.
example :: Array (Array (Int, Int))
example = fromList $ map fromList [[(1,2),(3,4)],[(5,6)]]
