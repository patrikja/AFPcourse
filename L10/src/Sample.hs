module Sample where

import Test.QuickCheck

-- sample :: Show a => Gen a -> IO ()





gI = arbitrary :: Gen Int
gC = arbitrary :: Gen Char  

sInt  = sample gI
sChar = sample gI

g1 = return 'a' :: Gen Char
g2 = oneof [return 'a', return 'b'] :: Gen Char
g3 = listOf $ choose ('a','z')

-- Even numbers
g4 = fmap (*2) gI
