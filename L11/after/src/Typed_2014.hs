{-# LANGUAGE GADTs #-}
module Typed where

-- Alternative 2: use a GADT

-- | The type of well-typed expressions. There is no way to
-- construct an ill-typed expression in this datatype.
data Expr t where
  Lit   :: a                               -> Expr a
  --  LitI  :: Int                             -> Expr Int
--LitC  :: Char                            -> Expr Char
--  LitB  :: Bool                            -> Expr Bool
  (:+)  ::              Expr Int-> Expr Int-> Expr Int
  (:==) :: Eq t =>      Expr t  -> Expr t  -> Expr Bool
  If    :: Expr Bool -> Expr t  -> Expr t  -> Expr t
  (:@)  :: Expr (a -> b) -> Expr a -> Expr b


-- | A type-safe evaluator.
eval :: Expr v -> v
eval (Lit x)         = x
--eval (LitI n)      = n
--eval (LitB b)      = b
eval (e1 :+ e2)    = eval e1 + eval e2
eval (e1 :== e2)   = eval e1 == eval e2
eval (If b t e)    = if eval b then eval t else eval e
eval (f :@ x)      = (eval f) (eval x)

eOK :: Expr Int
eOK  = If (Lit False) (Lit 1) (Lit 2 :+ Lit 1736)
--eOK  = If (LitB False) (LitI 1) (LitI 2 :+ LitI 1736)
-- eBad = If (LitB False) (LitI 1) (LitI 2 :+ LitB True)
