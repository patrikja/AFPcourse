{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
import Data.Set (Set)
import qualified Data.Set as Set

data Expr = Var Name | App Expr Expr | Lam Name Expr
  deriving (Show, Data, Typeable)

type Name = String

-- All the boring cases (just one in this example) can be taken
-- care of at once. But gmapQr etc. are complicated and often
-- inefficient.  See NoGenerics for a nicer way to do it.
freeVars :: Expr -> Set Name
freeVars (Var x)   = Set.singleton x
freeVars (Lam x e) = Set.delete x $ freeVars e
freeVars e         = gmapQr Set.union Set.empty 
                            (mkQ Set.empty freeVars) e

-- Examples
e1, e2, e3 :: Expr
e1 = Lam "x" $ Var "x"
e2 = Lam "unused" $ Var "C"
e3 = Lam "x" $ Lam "y" $ Lam "z" $ 
     App (Var "x") (Var "z") `App` 
     App (Var "y") (Var "z")
