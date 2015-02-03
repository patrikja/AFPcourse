{-# LANGUAGE GADTs #-}

module Problem4 where

-- a)

data Expr t where
  Lit  :: Int -> Expr Int
  Plus :: Expr (Int -> Int -> Int)
  App  :: Expr (a -> b) -> Expr a -> Expr b

-- b)

eval :: Expr t -> t
eval (Lit n)     = n
eval Plus        = (+)
eval (App e1 e2) = (eval e1) (eval e2)

