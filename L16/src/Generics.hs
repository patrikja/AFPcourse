{-# LANGUAGE DeriveDataTypeable #-}
module Generics where
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

size :: Expr -> Int
size = gmapQr (+) 1 (mkQ 0 size)

-- Examples
e1, e2, e3, e4 :: Expr
e1 = Lam "x" $ Var "x"
e2 = Lam "unused" $ Var "C"
e3 = Lam "x" $ Lam "y" $ Lam "z" $ 
     App (Var "x") (Var "z") `App` 
     App (Var "y") (Var "z")
e4 = buildExpr 20
buildExpr :: Int -> Expr
buildExpr 0 = Var "0"
buildExpr 1 = Var "3"
buildExpr n = Lam (show n) $ App (buildExpr (n-1)) (buildExpr (n-2))


main :: IO ()
main = do mapM_ print $ map freeVars [e1, e2, e3, e4]
          mapM_ print $ map size     [e1, e2, e3, e4]

