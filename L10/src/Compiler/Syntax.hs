module Compiler.Syntax(Name, Expr(..), Command(..)) where

import Compiler.Value

type Name = String

data Expr
  = Var Name
  | Val Value
  | Uno Op1 Expr
  | Duo Op2 Expr Expr
  deriving (Eq, Show)

data Command
  = Skip
  | Name := Expr
  | Command :-> Command     -- unnecessarily ambiguous
  | If Expr Command Command
  | While Expr Command
  | Print Expr
  deriving (Eq) -- add Show to debug pretty-printer
