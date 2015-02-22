{-# LANGUAGE TypeFamilies #-}
module AssocTypes where
import qualified Expr   as E
import qualified Middle as M
import qualified Typed  as T

class Eval e where
  type Value e
  eval :: e -> Value e

instance Eval E.Expr where
  type  Value E.Expr      = E.Value
  eval = E.eval

instance Eval M.ExprB where
  type  Value M.ExprB     = Bool 
  eval = M.evalB

instance Eval M.ExprI where
  type  Value M.ExprI     = Int   
  eval = M.evalI

instance Eval M.TypedExpr where
  type  Value M.TypedExpr = E.Value
  eval = M.eval
  
instance Eval (T.Expr v) where
  type  Value (T.Expr v)  = v
  eval = T.eval  

main = do print $ eval E.eOK
          print $ eval M.eOK
          print $ eval T.eOK

{-
Take-home message: use a type family when you would like a type
to be a class method. But think twice - perhaps you don't really
need it.
-}

-- Alternative "stand-alone" syntax (can be used without a class):
type family Value' a
type instance Value' E.Expr      = E.Value
type instance Value' M.ExprB     = Bool
type instance Value' M.ExprI     = Int
type instance Value' M.TypedExpr = E.Value
type instance Value' (T.Expr v)  = v

