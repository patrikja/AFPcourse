{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 0 of the interpreter
module Interpreter0 where
import qualified Control.Applicative    as CA (Applicative(..))
import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI

-- | The simplest expression language imaginable.
data Expr = Lit Integer
          | Expr :+ Expr
  deriving (Show)

type Value = Integer

{- | A monad for evaluating expressions. Just the identity monad
   at this point. Newtyped for abstraction purposes, could just
   as well be @type Eval = CMI.Identity@
-}
newtype Eval a = Eval { unEval :: CMI.Identity a }
  deriving (Functor, CA.Applicative, Monad)
           -- using newtype deriving (Haskell extension)

runEval :: Eval a -> a
runEval = CMI.runIdentity
        . unEval

-- | A monadic evaluator.
eval :: Expr  -> Eval Value
eval (Lit n)   = error "TBD"
eval (a :+ b)  = error "TBD"







-- liftM2 :: (a->b->c) -> Eval a -> Eval b -> Eval c
-- liftM2 (+) :: Num a => Eval a -> Eval a -> Eval a

testExpr :: Expr
testExpr = Lit 1700  :+   Lit 38

test :: Value
test = runEval $ eval testExpr
