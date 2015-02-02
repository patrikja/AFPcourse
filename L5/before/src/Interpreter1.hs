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
type Env = Map Name Value -- Think of it as [(Name, Value)] or Name -> Maybe Value

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
lookupVar x = error "TBD"        
  -- Here CMR.ask :: Eval Env
  -- CMR.lookup :: Ord k => k -> Map k a -> Maybe a
                 
-- | We can extend the environment with a new binding for a local
-- computation.  Since we're using a reader monad we can be sure
-- that this binding does not escape outside its intended scope.
extendEnv :: Name -> Value -> Eval a -> Eval a
extendEnv = error "TBD"
-- In general:
-- > CMR.local :: (CMR.MonadReader r m) => (r -> r) -> m a -> m a


-- | The evaluator is extended by simply adding cases for the
-- two new constructs. None of the old stuff has to change.
eval :: Expr -> Eval Value
eval (Lit n)     = error "TBD"
eval (a :+ b)    = error "TBD"
eval (Var n)     = error "TBD"
eval (Let n e b) = error "TBD"

-- * Utilities: testing and parsing

testExpr :: Expr
testExpr = parse "let x=1+2; x+x"
test :: Value
test = runEval $ eval testExpr

-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
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

parse :: String -> Expr
parse s = case  P.parseExpr language s  of
  Left err  -> error (show err)
  Right x   -> x
