{-# LANGUAGE GADTs #-}
-- Types: read, understand and extend Haskell programs which use advanced type system features
--     Types.Class: type classes, newtypes, deriving, ...
--     Types.GADT: (generalised) algebraic datatypes & type families
--     Types.HOT: functors, monads and monad transformers 

import Control.Monad (liftM2)

data Expr t where
  C     :: Char -> Expr Char
  I     :: Int ->  Expr Int
  (:@)  :: Expr (a -> b) -> Expr a -> Expr b
  Nil        :: Expr [a]
  Cons       :: Expr (a -> [a] -> [a])
  Replicate  :: Expr (Int -> a -> [a])
  Length     :: Expr ([a] -> Int)
-- Added later
  V   :: Name -> Expr String

type Name = Int

eval :: Expr t -> t
eval (C c)      = c
eval (I i)      = i
eval (f :@ x)   = (eval f) (eval x)
eval Nil        = []
eval Cons       = (:)
eval Replicate  = replicate
eval Length     = length

test = eval (Replicate :@ (I 5) :@ Nil)

stringLit :: String -> Expr String
stringLit = foldr (\x e -> Cons :@ C x :@ e) Nil

-- Added later:
eval2 :: (Name -> Maybe String) -> Expr t -> Maybe t
eval2 lookup (V v)  = lookup v
eval2 l (C c)       = Just c
eval2 l (I i)       = Just i
eval2 l (f :@ x)    = liftM2 ($) (eval2 l f) (eval2 l x)
eval2 l Nil         = Just []
eval2 l Cons        = Just (:)
eval2 l Replicate   = Just replicate
eval2 l Length      = Just length

-- Changes: 
-- * new constructor for Int-labelled variables
-- * Monadic (to handle variable lookup failure)
-- * eval2 has a new case for the variables

test2 = eval2 (const (Just "hej")) (Replicate :@ (Length :@ (V 1)) :@ V 1)

----------------------------------------------------------------
