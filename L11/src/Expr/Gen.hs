module Expr.Gen where
import Expr

import Test.QuickCheck
import Control.Applicative

instance Arbitrary Expr where
  arbitrary = sized arbExpr
-- Exercise1: generate "well-typed" expressions (in ExprB & ExprI)
arbExpr :: Int -> Gen Expr
arbExpr n | n <= 0 = basecases
arbExpr n | otherwise = oneof
    [ basecases
    , (:+)  <$> arbExpr2 <*> arbExpr2 
    , (:==) <$> arbExpr2 <*> arbExpr2 
    , If    <$> arbExpr3 <*> arbExpr3 <*> arbExpr3
    ]
  where arbExpr2 = arbExpr (n `div` 2)  
        arbExpr3 = arbExpr (n `div` 3)  

basecases :: Gen Expr
basecases = oneof [ LitI <$> arbitrary
                  , LitB <$> arbitrary ]
