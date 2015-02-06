module Compiler.TypeCheck where
import Compiler.Syntax      (Command(..), Expr(..), Name)

checkC :: Command -> State Env ()
checkC Skip         = return ()
checkC (n := e)     = do t <- checkE e; modify (insert n t)
checkC (c1 :-> c2)  = checkC c1 >> checkC c2
checkC (If b t e)   = do TBool <- checkE b
                         env <- get
                         checkC t
                         tenv <- get
                         put env
                         checkC e
                         eenv <- get
                         guard $ tenv == eenv
                         return tenv
                         
-- Complicated                         
