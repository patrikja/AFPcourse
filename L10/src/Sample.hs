module Sample where

import Test.QuickCheck

-- sample :: Show a => Gen a -> IO ()

gI :: Gen Int
gI = arbitrary
gC :: Gen Char  
gC = arbitrary

sInt :: IO ()
sInt  = sample gI
sChar :: IO ()
sChar = sample gC

g1 :: Gen Char
g1 = return 'a'
g2 :: Gen Char
g2 = oneof [return 'a', return 'b']
g3 :: Gen String
g3 = listOf $ choose ('a','z')

-- Even numbers
g4 :: Gen Int
g4 = fmap (*2) gI
