{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 1 of the interpreter
module Interpreter1 where

import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR -- new

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Parser as P (parseExpr, Language (..))

-- | A more interesting language with variables and let bindings.
data Expr = Lit Integer
          | Expr :+ Expr
          | Var Name            -- new
          | Let Name Expr Expr  -- new
  deriving (Show)

type Name  = String
type Value = Integer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | The evaluation monad now keeps track of passing around
-- the environment.
newtype Eval a = Eval { unEval :: CMR.ReaderT Env
                                    CMI.Identity
                                      a }
  deriving (Monad, CMR.MonadReader Env)

runEval :: Eval a -> a
runEval = CMI.runIdentity
        . startReaderFrom emptyEnv
        . unEval

startReaderFrom :: env -> CMR.ReaderT env m a -> m a
startReaderFrom = flip CMR.runReaderT
  -- > CMR.runReaderT :: CMR.ReaderT env m a ->
  -- >                              (env -> m a)

-- * Environment manipulation

-- | Looking up the value of a variable in the enviroment.
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- CMR.ask
  case Map.lookup x env of
    Nothing -> fail ("lookupVar: unbound variable: " ++ x)
    Just v  -> return v
-- Here CMR.ask :: Eval Env

-- | We can extend the environment with a new binding for a local
-- computation.  Since we're using a reader monad we can be sure
-- that this binding does not escape outside its intended scope.
extendEnv :: Name -> Value -> Eval a -> Eval a
extendEnv x v = CMR.local (Map.insert x v)
-- In general:
-- > CMR.local :: (CMR.MonadReader r m) => (r -> r) -> m a -> m a


-- | The evaluator is extended by simply adding cases for the
-- two new constructs. None of the old stuff has to change.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+ b)       = CM.liftM2 (+) (eval a) (eval b)
eval (Var x)        = lookupVar x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  extendEnv x v1 (eval e2)

-- * Utilities: testing and parsing

testExpr = parse "let x=1+2; x+x"
test = runEval $ eval testExpr

-- | The parser is parameterised over the abstract syntax.
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = error "language: not implemented: new"
  , P.lDeref  = error "language: not implemented: !"
  , P.lAssign = error "language: not implemented: :="
  , P.lCatch  = error "language: not implemented: catch"
  }

parse s = case  P.parseExpr language s  of
  Left err  -> error (show err)
  Right x   -> x
