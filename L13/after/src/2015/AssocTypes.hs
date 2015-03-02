{-# LANGUAGE TypeFamilies #-}
module AssocTypes where
import qualified Expr   as E
import qualified Middle as M
import qualified Typed  as T

-- Survey of our evals:
someEvals :: ( E.Expr      -> E.Value
             , M.ExprB     -> Bool
             , M.ExprI     -> Int
             , M.TypedExpr -> E.Value
             , T.Expr v    -> v
             )
someEvals = (E.eval, M.evalB, M.evalI, M.eval, T.eval)

-- Could we capture all of these instances in a type class? Note that
-- the value type varies with the expression type.

class Eval e where              type Value e;                  eval :: e -> Value e
instance Eval E.Expr     where  type Value E.Expr = E.Value;   eval = E.eval
instance Eval M.ExprB    where  type Value M.ExprB = Bool;     eval = M.evalB
instance Eval M.ExprI    where  type Value M.ExprI = Int;      eval = M.evalI
instance Eval (T.Expr t) where  type Value (T.Expr t) = t;     eval = T.eval

main = do print $ eval E.eOK
          print $ eval M.eOK
          print $ eval T.eOK
