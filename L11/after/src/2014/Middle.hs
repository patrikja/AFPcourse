{-# LANGUAGE GADTs #-}
module Middle where
import qualified Expr as E

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

evalB :: ExprB    ->  Bool
evalB (LitB b)     =  b
evalB (EqI e1 e2)  =  (evalI e1) == (evalI e2)
evalB (EqB e1 e2)  =  (evalB e1) == (evalB e2)
evalB (IfB b t e)  =  if (evalB b) then (evalB t) else (evalB e)

evalI :: ExprI    ->  Int
evalI (LitI n)     =  n
evalI (e1 :+ e2)   =  evalI e1 + evalI e2
evalI (IfI b t e)  =  if (evalB b) then (evalI t) else (evalI e)


data TypedExpr = TInt ExprI | TBool ExprB
-- data Type = TInt | TBool
eval :: TypedExpr -> E.Value
eval (TInt  ei) = E.VInt  (evalI ei)
eval (TBool eb) = E.VBool (evalB eb)

infer :: E.Expr -> Maybe TypedExpr
infer (E.LitB b)    = return (TBool (LitB b))
infer (E.LitI i)    = return (TInt  (LitI i))
infer (e1 E.:+ e2)  = do
  TInt ei1 <- infer e1
  TInt ei2 <- infer e2
  -- e1i, e12 :: ExprI
  return (TInt (ei1 :+ ei2))
infer (e1 E.:== e2) = error "TBD"
infer (E.If b t e)  = error "TBD"

