{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module FunDeps where
import qualified Expr   as E
import qualified Middle as M
import qualified Typed  as T

class     Eval e v  |  e -> v       where  eval :: e -> v
instance  Eval E.Expr      E.Value  where  eval = E.eval           
instance  Eval M.ExprB     Bool     where  eval = M.evalB          
instance  Eval M.ExprI     Int      where  eval = M.evalI
instance  Eval M.TypedExpr E.Value  where  eval = M.eval
instance  Eval (T.Expr v)  v        where  eval = T.eval  

main :: IO ()
main = do print $ eval E.eOK
          print $ eval M.eOK
          print $ eval T.eOK
