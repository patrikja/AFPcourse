module Lect1 where
import Data.Maybe (isNothing)

-- An example of a pure function returning an Int

f :: String -> Int
f xs = case reads xs of
  []             -> 1738
  ((i,_rest):_)  -> i+1

-- An example of a pure function returning an IO action (which in turn
-- returns an Int). Sometimes called an "impure function returning an Int".

g :: String -> IO Int
g xs = do putStrLn xs
          ys <- getLine
          return (f ys)

-- An IO action can be executed several times and can give different results 

testg :: IO [Int]
testg = let a = g "Shoesize?"
        in do s1 <- a
              s2 <- a
              s3 <- a
              return [s1, s2, s3]      

hello :: IO ()
hello =
  do putStrLn "Hello! What is your name?"
     name <- getLine
     putStrLn ("Hi, " ++ name ++ "!")

printTable1 :: [String] -> IO ()
printTable1 = prnt 1    -- Note the use of partial application
 where
  prnt i []      = return ()
  prnt i (x:xs)  = do  putStrLn (show i ++ ": " ++ x)
                       prnt (i+1) xs

lussekatter = ["1g saffran", "1kg (17dl) vetemjöl", "5dl mjölk", 
               "250g mager kesella", "50g jäst", "1.5dl socker", "0.5tsk salt"]
testTable1 = printTable1 lussekatter

printTable2 :: [String] -> IO ()
printTable2 xs =
  sequence_ [ putStrLn (show i ++ ":" ++ x)
            | (x,i) <- xs `zip` [1..length xs]
            ]   

testTable2 = printTable2 lussekatter

fun :: Maybe Int -> Int
fun mx  | isNothing mx   = 0
        | otherwise      = x + 3
 where
  x = fromJust mx

fromJust :: Maybe a -> a -- also available in module Data.Maybe
fromJust (Just x)  = x
fromJust Nothing   = error "fromJust: Nothing is not allowed"

testFun = (fun (Just 100), fun Nothing)

----------------

expn :: Integer -> Integer
expn n | n <= 1    = 1
       | otherwise = expn (n-1) + expn (n-2)

choice :: Bool -> a -> a -> a
choice False  f  t  =  f
choice True   f  t  =  t

testChoice1 = choice False 17 (expn 99)
testChoice2 = choice False 17 (error "Don't touch me!")

-- Laziness
testLazy1 = choice False 17 undefined
testLazy2 = head [3, undefined, 17]
testLazy3 = head (3:4:error "no tail")
testLazy4 = head [error "no first elem", 17, 13]
testLazy5 = head (error "no list at all")

----------------

strange :: Bool -> Integer
strange False = 17
strange True  = 17

testStrange = strange undefined

----------------
f' :: Integer -> Integer
f' x = (x - 2) * 10

foo :: Integer -> Integer
foo x = f' x + f' x

bar :: Integer -> Integer -> Integer
bar x y = f' 17 + x + y

testBar =  bar 1 2 + bar 3 4

----------------

testInfList1 n = take n [3..]
testInfList2 xs = xs `zip` [1..]

----

printTable3 :: [String] -> IO ()
printTable3 xs =
  sequence_ [ putStrLn (show i ++ ":" ++ x)
            | (x,i) <- xs `zip` [1..]
            ]
testTable3 = printTable3 lussekatter

----------------

-- the real iterate is defined in the stadard prelude
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

testIterate :: [Integer]
testIterate = iterate' (2*) 1
testIt :: [Integer]
testIt = take 12 testIterate

repeat' :: a -> [a]
repeat' x = x : repeat' x

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

repeat'' :: a -> [a]
repeat'' = iterate id

cycle'' :: [a] -> [a]
cycle'' xs = concat (repeat xs)

----------------
-- *** skip down to Labyrinth in lack of time

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x)

group :: Int -> [a] -> [[a]]
group n = takeWhile (not . null)
        . map (take n)
        . iterate (drop n)

primes :: [Integer]
primes = sieve [2..]
 where
  sieve (p:xs)  = p : sieve [ y | y <- xs, y `mod` p /= 0 ]
  sieve []      = error "sieve: empty list is impossible"

----------------

data Labyrinth
  = Crossroad
  { what  :: String
  , left  :: Labyrinth
  , right :: Labyrinth
  }
labyrinth :: Labyrinth
labyrinth = start
 where
  start  = Crossroad "start"  forest town
  town   = Crossroad "town"   start  forest
  forest = Crossroad "forest" town   exit
  exit   = Crossroad "exit"   exit   exit

showLabyrinth :: Labyrinth -> String
showLabyrinth (Crossroad _label _left _right) = error "Exercise!"

