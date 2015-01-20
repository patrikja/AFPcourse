module TestDSL where
import DSL
import Test.QuickCheck -- only used for testing

----------------------------------------------------------------
-- Testing code: not part of the exam.
  
instance Arbitrary C where
  arbitrary = sized arbC
  
arbC :: Int -> Gen C  
arbC 0 = elements [false, true]
arbC n = frequency [ (1, fmap inv   (arbC (n-1)))
                   , (1, fmap delay (arbC (n-1)))
                   , (1, fmap ands  (arbListC n))
                   , (1, fmap ors   (arbListC n))
                   ]
  
arbListC :: Int -> Gen [C]
arbListC n = do len <- choose (1, n `div` 2)
                let m = n `div` len
                vectorOf len  (arbC m)

test = take 5 $ run toggle

-- limit the list length to keep the running time down
q f = quickCheck (forAll (choose (0,100)) f)

main = do q prop_inv
          quickCheck prop_delay0
          q prop_delay
          q prop_toggle
          q prop_true
