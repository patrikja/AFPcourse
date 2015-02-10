module Testing where
import Test.QuickCheck
import qualified Data.Set as Set
import Control.Monad(liftM, liftM2)
import Data.List((\\))
import InsertionSort
{-
quickCheck :: Testable prop => prop -> IO ()

class Testable prop where
  property :: prop -> Property

instance Testable Bool
instance  (Arbitrary a, Show a, Testable prop) =>
            Testable (a -> prop)

class Testable prop where
  property :: prop -> Property

property :: Testable prop => prop -> Property

instance Testable Bool
instance  (Arbitrary a, Show a, Testable prop) =>
            Testable (a -> prop)

-}
sortAscending :: Bool
sortAscending = isort [2,1] == [1,2]

sortDescending :: Bool
sortDescending = isort [2,1] == [2,1]

test1 :: IO ()
test1 = quickCheck sortAscending
test2 :: IO ()
test2 = quickCheck sortDescending

sortPreservesLength :: ([Int] -> [Int]) -> [Int] -> Bool
sortPreservesLength sort xs = length (sort xs) == length xs

test3 :: IO ()
test3 = quickCheck (sortPreservesLength isort)

setSort :: [Int] -> [Int]
setSort = Set.toList . Set.fromList

test4 :: IO ()
test4 = quickCheck (sortPreservesLength setSort)

sortOrders :: [Int] -> Bool
sortOrders xs = ordered (isort xs)

sortPreservesElements :: [Int] -> Bool
sortPreservesElements xs = sameElements xs (isort xs)

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

-- collect :: (Testable prop, Show a) => a -> prop -> Property

sPLen :: [Int] -> Bool
sPLen = sortPreservesLength isort
test5 :: IO ()
test5 =  quickCheck (\ xs -> collect (null xs) (sPLen xs))
test6 :: IO ()
test6 =  quickCheck (\ xs -> collect (length xs `div` 10) (sPLen xs))
test7 :: IO ()
test7 =  quickCheck (\ xs -> collect xs (sPLen xs))

implies :: Bool -> Bool -> Bool
implies x y = not x || y

insertPreservesOrdered :: Int -> [Int] -> Bool
insertPreservesOrdered x xs =
  ordered xs `implies` ordered (insert x xs)

test8 :: IO ()
test8 = quickCheck insertPreservesOrdered

iPO :: Int -> [Int] -> Bool
iPO = insertPreservesOrdered
test9 :: IO ()
test9 = quickCheck (\x xs -> collect (ordered xs) (iPO x xs))

-- (==>) :: (Testable prop) => Bool -> prop -> Property

-- instance Testable Property

iPO2 :: Int -> [Int] -> Property
iPO2 x xs = ordered xs ==> ordered (insert x xs)

test92 :: IO ()
test92 = quickCheck (\ x xs -> collect (ordered xs) (iPO2 x xs))

{-
data Args = Args {
  replay     :: Maybe (StdGen, Int)
  maxSuccess :: Int
  maxDiscard :: Int
  maxSize    :: Int
  }
stdArgs :: Args
stdArgs = Args {replay     = Nothing,
                maxSuccess = 100,
                maxDiscard = 500,
                maxSize    = 100}
quickCheckWith :: Testable prop => Args -> prop -> IO ()

class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]

choose     ::  Random a => (a,a) -> Gen a
oneof      ::  [Gen a] -> Gen a
frequency  ::  [(Int, Gen a)] -> Gen a
elements   ::  [a] -> Gen a
sized      ::  (Int -> Gen a) -> Gen a

instance Arbitrary Bool where
  arbitrary = choose (False, True)

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary =  do
                 x  <-  arbitrary
                 y  <-  arbitrary
                 return (x,y)
-}

data Dir = North | East | South | West
instance Arbitrary Dir where
  arbitrary = elements [North, East, South, West]

{-
instance Arbitrary Int where
  arbitrary = choose (-20,20)

instance Arbitrary Int where
  arbitrary = sized (\ n -> choose (-n,n))
-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = arbTreeBad
-}

arbTreeBad :: Arbitrary a => Gen (Tree a)
arbTreeBad =
    frequency  [  (1, liftM   Leaf  arbitrary),
                  (2, liftM2  Node  arbitrary arbitrary) ]

{-
liftM   :: (a -> b)       ->  Gen a -> Gen b
liftM2  :: (a -> b -> c)  ->  Gen a -> Gen b -> Gen c
-}

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized arbitraryTree

arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree 0 = liftM Leaf arbitrary
arbitraryTree n = frequency [  (1, liftM   Leaf  arbitrary),
                               (4, liftM2  Node  t t) ]
  where t = arbitraryTree (n `div` 2)

-- See also GenTree.hs

-- shrink :: (Arbitrary a) => a -> [a]
