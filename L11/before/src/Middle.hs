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
evalB (LitB b)     =  error "evalB: TBD"
evalB (EqI e1 e2)  =  error "evalB: TBD"
evalB (EqB e1 e2)  =  error "evalB: TBD"
evalB (IfB b t e)  =  error "evalB: TBD"

evalI :: ExprI    ->  Int
evalI (LitI n)     =  error "evalI: TBD"
evalI (e1 :+ e2)   =  error "evalI: TBD"
evalI (IfI b t e)  =  error "evalI: TBD"


data TypedExpr = TInt ExprI | TBool ExprB
-- data Type = TInt | TBool
eval :: TypedExpr -> E.Value
eval (TInt  ei) = error "E.VInt  (evalI ei)"
eval (TBool eb) = error "E.VBool (evalB eb)"

infer :: E.Expr -> Maybe TypedExpr
infer (E.LitB b)    = error "TBD"
infer (E.LitI i)    = error "TBD"
infer (e1 E.:+ e2)  = error "TBD"
infer (e1 E.:== e2) = error "TBD"
infer (E.If b t e)  = error "TBD"

