{-# LANGUAGE GADTs #-}
module Expr where

-- | A simple expression language with integers and booleans.
-- Contains both well- and ill-typed expressions.
data Expr where
  LitI  :: Int                           -> Expr
  LitB  :: Bool                          -> Expr
  (:+)  ::         Expr      -> Expr     -> Expr
  (:==) ::         Expr      -> Expr     -> Expr
  If    :: Expr -> Expr      -> Expr     -> Expr

-- | Example expressions:
eOK, eBad  :: Expr
eOK  = If (LitB False) (LitI 1) (LitI 2 :+ LitI 1736)
eBad = If (LitB False) (LitI 1) (LitI 2 :+ LitB True)
eBad'= If (LitB False) (LitI 1) (LitB True)

-- | A value is an integer or a boolean. 
data Value = VInt Int | VBool Bool
  deriving (Show)
-- type Value = Either Int Bool

-- | Evaluating expressions. Things are a bit complicated
-- because we have to check that we get values of the right
-- types for the operations. Fails if the evaluated expression
-- isn't well-typed.
eval :: Expr -> Value
eval (LitI n)     = VInt n
eval (LitB b)     = VBool b
eval (e1 :+ e2)   = plus (eval e1) (eval e2)
eval (e1 :== e2)  = evalEq (eval e1) (eval e2)
eval (If b t e)   = evalIf (eval b) (eval t) (eval e)

plus :: Value -> Value -> Value
plus (VInt a) (VInt b) = VInt (a+b)
plus _        _        = error "plus: type error"

evalIf :: Value -> (Value -> Value -> Value)
evalIf (VBool b) = evalIf2 b
evalIf (VInt _)  = error "evalIf: type error"

evalIf2 :: Bool -> Value -> Value -> Value
evalIf2 b (VInt  t) (VInt  e) = VInt  (if b then t else e)
evalIf2 b (VBool t) (VBool e) = VBool (if b then t else e)
evalIf2 b _         _         = error "evalIf2: type error"

evalIf2' b t e = if b then t else e -- allows eBad'

evalEq :: Value -> Value -> Value
evalEq (VBool b1) (VBool b2) = VBool (b1==b2)
evalEq (VInt  b1) (VInt  b2) = VBool (b1==b2)
evalEq _          _          = error "evalEq: type error"
  
-- Pretty printing.
instance Show Expr where
  showsPrec p e = case e of
    LitI n       -> shows n

    LitB b       -> shows b

    e1 :+ e2     -> showParen (p > 2) $
      showsPrec 2 e1 . showString " + " . showsPrec 3 e2

    e1 :== e2    -> showParen (p > 1) $
      showsPrec 2 e1 . showString " == " . showsPrec 2 e2

    If e1 e2 e3  -> showParen (p > 0) $
      showString "if "    . shows e1 .
      showString " then " . shows e2 .
      showString " else " . shows e3

----------------------------------------------------------------
{-
import Control.Applicative((<*>),(<*),(*>), (<$>),(<$), (<|>))
import Data.Char(isDigit)
import Parser(P, parse, this, sat, chainLeft)
-}
{-
-- Parsing expressions. Uses a slightly modified version of our parser
-- library from lecture 4. Also goes crazy with the operators from
-- "Control.Applicative".  Exercise: check out these combinators.
type Token = String
instance Read Expr where
  readsPrec p s = [ (x, unwords ts)
                  | (x, ts) <- parse (exprP p) $ tokenize s ]
    where
      tokenize :: String -> [Token]
      tokenize "" = []
      tokenize s  = t : tokenize s'
        where [(t, s')] = lex s

      exprP :: Int -> P Token Expr
      exprP 0 = If <$> (this "if"   *> exprP 0)
                   <*> (this "then" *> exprP 0)
                   <*> (this "else" *> exprP 0)
            <|> exprP 1

      exprP 1 = (:==) <$> exprP 2 <*> (this "==" *> exprP 2)
            <|> exprP 2

      exprP 2 = chainLeft plusP (exprP 3)
        where plusP = (:+) <$ this "+"

      exprP _ = foldr1 (<|>)
        [ LitI . read  <$>  sat (all isDigit)
        , LitB . read  <$>  sat (`elem` ["True", "False"])
        , this "(" *> exprP 0 <* this ")"
        ]

-}

