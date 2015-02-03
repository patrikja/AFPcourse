module QCdefaulting where
import Test.QuickCheck

ordered (x : y : ys)  = x <= y  && ordered (y : ys)
ordered _             = True

{-
quickCheck ordered
-- Checks [()] due to the defaulting in ghci

verboseCheck ordered
-}

testBool = quickCheck (ordered :: [Bool] -> Bool)
