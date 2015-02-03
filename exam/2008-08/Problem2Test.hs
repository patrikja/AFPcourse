{-# LANGUAGE TypeSynonymInstances #-}

module Problem2Test where

import Control.Monad
import Test.QuickCheck
import Problem2c

-- Generators

string :: Gen String
string = do
  NonNegative n <- arbitrary 
  replicateM n $ elements "XYZ"

genIndent :: Gen Int
genIndent = sized $ \n -> resize (div n 3) $ do
  NonNegative n <- arbitrary
  return n

newtype Doc' = Doc' Doc

instance Show Doc' where
  show (Doc' d) = show d

instance Arbitrary Doc' where
  arbitrary = liftM Doc' $ sized arb
    where
      arb 0 = oneof [return empty, liftM text string]
      arb n = frequency
        [ (1, arb 0)
        , (1, liftM2 ($$) arb2 arb2)
        , (1, liftM2 (<>) arb2 arb2)
        , (1, liftM2 (<+>) arb2 arb2)
        , (1, liftM2 indent genIndent arb1)
        ]
        where
          arb2 = arb $ div n 2
          arb1 = arb $ n - 1
  shrink (Doc' d) = [ Doc' d' | d' <- shrinkDoc d ]

shrinkDoc Empty = []
shrinkDoc (Text s) = Empty : [ Text s' | s' <- shrink s ]
shrinkDoc (x :$$ y) = x : y : [ x' :$$ y  | x' <- shrinkDoc x ] ++
                              [ x  :$$ y' | y' <- shrinkDoc y ]
shrinkDoc (x :<> y) = x : y : [ x' :<> y  | x' <- shrinkDoc x ] ++
                              [ x  :<> y' | y' <- shrinkDoc y ]

-- Tests

width :: Doc -> Int
width d = case lines (render d) of
  []  -> 0
  xs  -> length (last xs)

nonEmpty :: Doc -> Bool
nonEmpty Empty = False
nonEmpty (Text _) = True
nonEmpty (x :$$ y) = nonEmpty x || nonEmpty y
nonEmpty (x :<> y) = nonEmpty x || nonEmpty y

x =~ y = render x == render y

prop_vassoc (Doc' d1) (Doc' d2) (Doc' d3) = (d1 $$ (d2 $$ d3)) =~ ((d1 $$ d2) $$ d3)
prop_hassoc (Doc' d1) (Doc' d2) (Doc' d3) = (d1 <> (d2 <> d3)) =~ ((d1 <> d2) <> d3)

prop_vh (Doc' d1) (Doc' d2) (Doc' d3) =
  nonEmpty d2 ==>
  ((d1 $$ d2) <> d3) =~ (d1 $$ (d2 <> d3))

prop_hv (Doc' d1) (Doc' d2) (Doc' d3) =
  nonEmpty d2 && nonEmpty d3 ==>
  (d1 <> (d2 $$ d3)) =~ ((d1 <> d2) $$ indent (width d1) d3)
