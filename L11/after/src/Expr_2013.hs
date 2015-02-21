{-# LANGUAGE GADTs #-}
module Expr where

-- | A simple expression language with integers and booleans.
-- Contains both well- and ill-typed expressions.
data Expr where
  LitN  :: Int                           -> Expr
  LitB  :: Bool                          -> Expr
  (:+)  ::         Expr      -> Expr     -> Expr
  (:==) ::         Expr      -> Expr     -> Expr
  If    :: Expr -> Expr      -> Expr     -> Expr

-- | Evaluating expressions. Things are a bit complicated
-- because we have to check that we get values of the right
-- types for the operations. Fails if the evaluated expression
-- isn't well-typed.
-- type Value = Either Bool Int
eval :: Expr -> Value
eval (LitN n)       = VInt n
eval (LitB b)       = VBool b
eval (e1 :+ e2)     = evalPlus (eval e1) (eval e2)
eval (e1 :== e2)    = evalEq   (eval e1) (eval e2)
eval (If e1 e2 e3)  = evalIf   (eval e1) (eval e2) (eval e3)

evalIf :: Value -> Value -> Value -> Value
evalIf (VBool b) vthen velse 
  | checkSameType vthen velse = if b then vthen else velse
evalIf vcond _ _ =  error ("evalIf: type error " ++ show vcond)
  
checkSameType _ _ = True

evalPlus :: Value -> Value -> Value
evalPlus (VInt x) (VInt y) = VInt (x + y)
evalPlus vx       vy       = 
  error ("evalPlus: type error " ++ show vx ++ " + " ++ show vy)

evalEq :: Value -> Value -> Value
evalEq (VInt x) (VInt y) = VBool (x == y)
evalEq (VBool x)(VBool y)= VBool (x == y)
evalEq vx       vy       = 
  error ("evalEq: type error " ++ show vx ++ " + " ++ show vy)

eOK, eBad  :: Expr
eOK  = If (LitB False) (LitN 1) (LitN 2 :+ LitN 1736)
eBad = If (LitB False) (LitN 1) (LitN 2 :+ LitB True)
eBad2= If (LitB False) (LitN 1) (LitB True)






-- | A value is an integer or a boolean.
data Value = VInt Int | VBool Bool
  deriving Show

-- Pretty printing.
instance Show Expr where
  showsPrec p e = case e of
    LitN n       -> shows n

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
        [ LitN . read  <$>  sat (all isDigit)
        , LitB . read  <$>  sat (`elem` ["True", "False"])
        , this "(" *> exprP 0 <* this ")"
        ]

-}
