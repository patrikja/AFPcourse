{-# LANGUAGE GADTs #-}
module Middle where
import qualified Expr as E

-- Alternative 1: create two datatypes ExprI and ExprB
data ExprI where
  LitI  :: Int                            -> ExprI
  (:+)  ::          ExprI     -> ExprI    -> ExprI
  IfI   :: ExprB -> ExprI     -> ExprI    -> ExprI
  deriving Show

data ExprB where
  LitB  :: Bool                           -> ExprB
  EqI   ::          ExprI     -> ExprI    -> ExprB
  EqB   ::          ExprB     -> ExprB    -> ExprB
  IfB   :: ExprB -> ExprB     -> ExprB    -> ExprB
  deriving Show

evalB :: ExprB    ->  Bool
evalB (LitB b)     =  b
evalB (EqI e1 e2)  =  (evalI e1) == (evalI e2)
evalB (EqB e1 e2)  =  (evalB e1) == (evalB e2)
evalB (IfB b t e)  =  if evalB b then evalB t else evalB e

evalI :: ExprI    ->  Int
evalI (LitI n)     =  n
evalI (e1 :+ e2)   =  evalI e1 + evalI e2
evalI (IfI b t e)  =  if evalB b then evalI t else evalI e

data TypedExpr = TInt  ExprI | TBool ExprB
{-
data TypedExpr = (TInt,  ExprI)
               | (TBool, ExprB)
  deriving Show
-}

data Type = TI | TB

eval :: TypedExpr -> E.Value
eval (TInt  ei) = E.VInt  $ evalI ei
eval (TBool eb) = E.VBool $ evalB eb

infer :: E.Expr -> Maybe TypedExpr
infer (E.LitB b)    = return $ TBool (LitB b)
infer (E.LitI i)    = return $ TInt  (LitI i)
infer (e1 E.:+ e2)  = do
  TInt ie1  <-  infer e1
  TInt ie2  <-  infer e2
  return $ TInt (ie1 :+ ie2)
infer (e1 E.:== e2) = do
  te1 <- infer e1
  te2 <- infer e2
  inferSameType te1 te2
infer (E.If b t e)  = error "TBD"

inferSameType :: TypedExpr -> TypedExpr -> Maybe TypedExpr
inferSameType (TInt  tei1) (TInt  tei2) = return $ TBool $ EqI tei1 tei2
inferSameType (TBool teb1) (TBool teb2) = return $ TBool $ EqB teb1 teb2
inferSameType _            _            = Nothing

checkSameType :: TypedExpr -> TypedExpr -> Bool
checkSameType (TInt  _) (TInt  _) = True
checkSameType (TBool _) (TBool _) = True
checkSameType _         _         = False
