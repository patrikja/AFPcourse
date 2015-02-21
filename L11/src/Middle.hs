{-# LANGUAGE GADTs #-}
module Middle where
import qualified Expr as E
import Expr.Gen () -- only instances

import Data.Maybe
import Test.QuickCheck

-- Alternative 1: create two datatypes ExprI and ExprB
data ExprI where
  LitI  :: Int                            -> ExprI
  (:+)  ::          ExprI     -> ExprI    -> ExprI
  IfI   :: ExprB -> ExprI     -> ExprI    -> ExprI

data ExprB where
  LitB  :: Bool                           -> ExprB
  EqI   ::          ExprI     -> ExprI    -> ExprB
  EqB   ::          ExprB     -> ExprB    -> ExprB
  IfB   :: ExprB -> ExprB     -> ExprB    -> ExprB

evalB :: ExprB -> Bool
evalB (LitB b)     =  b
evalB (EqI e1 e2)  =  evalI e1  ==  evalI e2
evalB (EqB e1 e2)  =  evalB e1  ==  evalB e2
evalB (IfB b t e)  =  if evalB b then evalB t else evalB e

evalI :: ExprI -> Int
evalI (LitI n)     =  n
evalI (e1 :+ e2)   =  evalI e1  +  evalI e2
evalI (IfI b t e)  =  if evalB b then evalI t else evalI e

data TypedExpr = TInt ExprI | TBool ExprB

eval :: TypedExpr -> E.Value
eval (TInt  ei) = E.VInt  (evalI ei)
eval (TBool eb) = E.VBool (evalB eb)

infer :: E.Expr -> Maybe TypedExpr
infer (E.LitB b)    = Just (TBool (LitB b))
infer (E.LitI i)    = Just (TInt  (LitI i))
infer (e1 E.:+ e2)  = do
  TInt i1 <- infer e1
  TInt i2 <- infer e2
  return (TInt (i1 :+ i2))
infer (e1 E.:== e2) = do
  te1 <- infer e1
  te2 <- infer e2
  inferEq te1 te2
infer (E.If b t e)  = do
  tb <- infer b
  tt <- infer t
  te <- infer e
  inferIf tb tt te

inferEq :: TypedExpr -> TypedExpr -> Maybe TypedExpr
inferEq (TInt  e1) (TInt  e2) = Just (TBool (EqI e1 e2))
inferEq (TBool e1) (TBool e2) = Just (TBool (EqB e1 e2))
inferEq _          _          = Nothing

inferIf :: TypedExpr -> TypedExpr -> TypedExpr -> Maybe TypedExpr
inferIf (TBool b) tt te = inferIf2 b tt te
inferIf _         _  _  = Nothing

inferIf2 :: ExprB -> TypedExpr -> TypedExpr -> Maybe TypedExpr
inferIf2 b (TInt  e1) (TInt  e2) = Just (TInt  (IfI b e1 e2))
inferIf2 b (TBool e1) (TBool e2) = Just (TBool (IfB b e1 e2))
inferIf2 _ _          _          = Nothing

-- data Type = TInt | TBool

prop_eval :: E.Expr -> Property
prop_eval e = let  mte        = infer e
                   wellTyped  = isJust mte
                   Just te    = mte
              in wellTyped ==>
                 E.eval e == eval te

eOK :: ExprI
eOK  = IfI (LitB False) (LitI 1) (LitI 2 :+ LitI 1736)
-- eBad = IfI (LitB False) (LitI 1) (LitI 2 :+ LitB True)

-- | Check that the evals agree for well-typed terms
main = run 50 prop_eval
  -- quickCheck prop_eval

run n = quickCheckWith stdArgs{ maxSuccess = n }

-- Try with hpc (Haskell program coverage)
