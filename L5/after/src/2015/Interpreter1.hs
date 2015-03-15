{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 1 of the interpreter
module Interpreter1 where

import qualified Control.Applicative    as CA (Applicative(..))
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
  deriving (Functor, CA.Applicative,
            Monad, CMR.MonadReader Env)
-- Eval a ~= Env -> a

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
    Nothing -> fail $ "Variable " ++ x ++ " not found."
    Just v  -> return v
  -- Here CMR.ask :: Eval Env
  -- Map.lookup :: Ord k => k -> Map k a -> Maybe a

-- | We can extend the environment with a new binding for a local
-- computation.  Since we're using a reader monad we can be sure
-- that this binding does not escape outside its intended scope.
localScope :: Name -> Value -> Eval a -> Eval a
localScope n v = CMR.local (Map.insert n v)
  -- CMR.local (Map.insert n v) :: Eval a -> Eval a

-- In general:
-- > CMR.local :: (CMR.MonadReader r m) => (r -> r) -> m a -> m a


-- | The evaluator is extended by simply adding cases for the
-- two new constructs. None of the old stuff has to change.
eval :: Expr -> Eval Value
eval (Lit n)     = return n
eval (a :+ b)    = CM.liftM2 (+) (eval a) (eval b)
eval (Var n)     = lookupVar n
eval (Let n e b) = do -- evaluate b with (n |-> eval e) in the context
   ev <- eval e
   localScope n ev (eval b)
  

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
