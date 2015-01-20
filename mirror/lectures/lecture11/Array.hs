{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Array where

-- Some preliminaries (for stronger type checking)
newtype Index = Index {unIndex :: Int} 
  deriving (Num, Eq, ArrayElem, Ord, Enum)

newtype Size  = Size  {unSize  :: Int} 
  deriving (Num, Eq, ArrayElem)

toIndex  :: Size -> Index
toIndex (Size n) = Index n

maxIndex :: Size -> Index
maxIndex n = toIndex n - 1

toSize :: Index -> Size
toSize (Index i) = Size i

{- A type family is like a function on the type level where you
can pattern match on the type arguments. An important difference
is that it's /open/ which means that you can add more equations
to the function at any time.

In this example we will implement a type family of "efficient"
array implementations.

Type families are often used as "associated types" - a type
member in a class, much like the class methods are function
members of the class.  
-}

-- The type signature for our family / associated datatype
data family Array a
-- The "family members" will be arrays of ints, pairs, and nested
-- arrays.

-- | A class of operations on the array family. We'll make one
--   instance per clause in the definition of the family.
class ArrayElem a where
  (!)      :: Array a -> Index -> a
  -- | @slice a (i,n)@ is the slice of @a@ starting at 
  --   index @i@ with @n@ elements.
  slice    :: Array a -> (Index, Size) -> Array a
  size     :: Array a -> Size
  fromList :: [a] -> Array a

-- | Converting an array to a list.
toList :: ArrayElem a => Array a -> [a]
toList a = [ a ! i | i <- [0..maxIndex (size a)] ]

----------------
-- * Int arrays. 
-- | We cheat with the implementation of Int arrays to make
-- implementations of the functions easy.
type SuperEfficientByteArray = [Int] -- cheating
newtype instance Array Int   = ArrInt SuperEfficientByteArray 

-- | Here is a style of instance declaration which just binds
-- names and delegates all the work to helper functions. This
-- style makes the types of the instatiated member functions
-- more visible which can help in documenting and developing the
-- code. (Haskell does not allow type signatures in instance
-- declarations.)
instance ArrayElem Int where
  (!)      = indexInt
  slice    = sliceInt
  size     = sizeInt
  fromList = fromListInt

indexInt :: Array Int -> Index -> Int
indexInt (ArrInt a) (Index i) = a !! i

sliceInt :: Array Int -> (Index, Size) -> Array Int
sliceInt (ArrInt a) (Index i, Size n) =
    ArrInt (take n (drop i a))

sizeInt  :: Array Int -> Size
sizeInt (ArrInt a) = Size (length a)

fromListInt :: [Int] -> Array Int
fromListInt = ArrInt

----------------
-- Arrays of Sizes or Indices are just the same as arrays of ints
newtype instance Array Size  = ArrSize  (Array Int)
newtype instance Array Index = ArrIndex (Array Int)
-- instance declarations are derived at the definition of Size and
-- Index

----------------
-- | Arrays of pairs are pairs of arrays. Mostly just lifting
-- the operations to work on pairs.
newtype instance Array (a, b) = ArrPair (Array a, Array b)

{-

-- This is a working and short definition, which may be
-- preferrable as the end result, but which may be difficult to
-- read and understand if you are not used to Haskell.

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  ArrPair (as, bs) ! i = (as ! i, bs ! i)

  slice (ArrPair (as, bs)) (i, n) =
    ArrPair (slice as (i, n), slice bs (i, n))

  size (ArrPair (as, _)) = size as

  fromList xs = ArrPair (fromList as, fromList bs)
    where (as, bs) = unzip xs
-}

-- | This is the expanded version showing all the types
instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  (!)      = indexPair
  slice    = slicePair
  size     = sizePair
  fromList = fromListPair

indexPair :: (ArrayElem a, ArrayElem b) => 
  Array (a, b) -> Index -> (a, b)
indexPair (ArrPair (as, bs)) i = (as ! i, bs ! i)

slicePair :: (ArrayElem a, ArrayElem b) =>
  Array (a, b) -> (Index, Size) -> Array (a, b)
slicePair (ArrPair (as, bs)) (i, n) =
  ArrPair (slice as (i, n), slice bs (i, n))

sizePair :: ArrayElem a => Array (a, b) -> Size
sizePair (ArrPair (as, _)) = size as

fromListPair :: (ArrayElem a, ArrayElem b) => 
  [(a, b)] -> Array (a, b)
fromListPair xs = ArrPair (fromList as, fromList bs)
    where (as, bs) = unzip xs

-- | Nested arrays. Here we have to work a little bit.  A nested
-- array is implemented as a flat array together with an array
-- of offsets (indices) and sizes of the subarrays.
data instance Array (Array a) = ArrNested (Array a) 
                                          (Array (Index, Size))

-- Exercise: what invariant must hold for this to work?
-- Implement a property checking it.

instance ArrayElem a => ArrayElem (Array a) where
  (!)      = indexNested
  slice    = sliceNested
  size     = sizeNested
  fromList = fromListNested

-- Indexing is just slicing the flat array.
indexNested :: (ArrayElem a) => 
  Array (Array a) -> Index -> Array a
indexNested (ArrNested as segs) i = slice as (segs ! i)

-- | Slicing is slicing the flat array and the segment array,
-- but we have to do some work to figure out how to slice the
-- flat array.
sliceNested :: (ArrayElem a) =>
  Array (Array a) -> (Index, Size) -> Array (Array a)
sliceNested (ArrNested as segs) (i, n) =
  ArrNested (slice as   (j, m)) 
            (fixInvariant $ slice segs (i, n))
  where
    fixInvariant = fromList . map (mapFst ((-j)+)) . toList
    (j, _) = segs ! i
    m      = sum [ snd (segs ! k) -- second components store sizes
                 | k <- [i..n'-1] ]
    n' = min (i+toIndex n) (toIndex $ size segs) 
         -- don't look too far
-- Exercise: Refactor to avoid summing - something like 
--    m' = toSize (fst (segs ! (i + toIndex n))  - j)

mapFst :: (a->a') -> (a, b) -> (a', b)
mapFst       f       (a, b) = (f a, b)
         

sizeNested :: Array (Array a) -> Size
sizeNested (ArrNested _ segs) = size segs

-- Creating the flat array isn't very nicely done. We shouldn't
-- have to convert our arrays back to lists to do it.  Solution
-- (exercise): add a concatenation operation on arrays.
fromListNested :: (ArrayElem a) => [Array a] -> Array (Array a)
fromListNested xss = 
    ArrNested (fromList $ concatMap toList xss) -- Bad!
              (fromList $ tail $ scanl seg (0,0) xss)
  where seg (i, n) a = (i + toIndex n, size a)

----------------------------------------------------------------

instance Show Size  where 
  showsPrec p (Size n)  = showParen (p > 0) $
    showString "Size " . showsPrec 1 n
instance Show Index where 
  showsPrec p (Index n) = showParen (p > 0) $
    showString "Index " . showsPrec 1 n
