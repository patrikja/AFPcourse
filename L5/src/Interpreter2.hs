{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 2 of the interpreter
module Interpreter2 where

import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS -- new

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Parser as P (parseExpr, Language (..))

-- | Even more interesting stuff: mutable references!
data Expr = Lit Integer
          | Expr :+ Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr         -- new
          | Deref Expr          -- new
          | Expr := Expr        -- new
  deriving (Show)

type Name   = String
type Value  = Integer
type Ptr    = Value
  -- ^ dangerous language: any 'Value' can be used as a 'Ptr'

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | We need to keep track of the store containing the values of
-- our references. We also remember the next unused pointer.
data Store = Store { nextPtr :: Ptr
                   , heap    :: Map Ptr Value
                   }

emptyStore :: Store
emptyStore = Store 0 Map.empty

-- | The store needs to be updated globally in a program so we
-- use a state monad to pass the store around.

newtype Eval a = Eval { unEval :: CMS.StateT Store
                                    (CMR.ReaderT Env
                                      CMI.Identity)
                                        a }
  deriving (Monad,  CMS.MonadState Store,
                    CMR.MonadReader Env  )
{- ^ Explaining and expanding the type
  CMS.StateT s m' a  ~=  s -> m' (a, s)
  CMR.ReaderT e m a  ~=  e -> m a
  CMI.Identity a     ~=  a
  => Eval a  ~=
     s -> m' (a, s)     ~= {- where m' = CMR.ReaderT Env m -}
     s -> e -> m (a,s)  ~= {- where m  = CMI.Identity      -}
     s -> e -> (a,s)
-}

runEval :: Eval a -> a
runEval = CMI.runIdentity
        . startReaderFrom emptyEnv
        . startStateFrom  emptyStore
        . unEval

startStateFrom :: Monad m => state -> CMS.StateT state m a -> m a
startStateFrom = flip CMS.evalStateT
  -- > CMS.evalStateT :: Monad m => CMS.StateT state m a ->
  -- >                                (state -> m a)

startReaderFrom :: env -> CMR.ReaderT env m a -> m a
startReaderFrom = flip CMR.runReaderT
  -- > CMR.runReaderT :: CMR.ReaderT env m a ->
  -- >                              (env -> m a)

-- * Environment manipulation
--   No changes necessary from 'Interpreter1'

-- | Looking up the value of a variable in the enviroment.
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- CMR.ask
  case Map.lookup x env of
    Nothing -> fail ("lookupVar: unbound variable: " ++ x)
    Just v  -> return v

extendEnv :: Name -> Value -> Eval a -> Eval a
extendEnv x v = CMR.local (Map.insert x v)

-- * Store manipulation (new)

-- | Create a new reference containing the given value.
newRef :: Value -> Eval Ptr
newRef v = do
  s <- CMS.get
  let ptr = nextPtr s
      s'  = s { nextPtr = ptr + 1
              , heap    = Map.insert ptr v (heap s)
              }
  CMS.put s'
  return ptr


-- | Get the value of a reference. Crashes with our own
-- "segfault" if given a non-existing pointer.
deref :: Ptr -> Eval Value
deref p = do
  h <- CMS.gets heap -- remember *heap :: Store -> Map Ptr Value*
  case Map.lookup p h of
    Nothing -> fail "deref: segmentation fault"
    Just v  -> return v

-- | Updating the value of a reference. Has no effect if the
-- reference doesn't exist. (Exercise: Maybe that's not the best
-- semantics... what would be a better one?)
(=:) :: Ptr -> Value -> Eval Value
p =: v = do
  CMS.modify $ \s -> s { heap = Map.adjust (const v) p (heap s) }
  return v
-- Map.adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a

-- | As before we only need to add cases for the new con-
-- structors to the evaluator. No need to change the old stuff.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+ b)       = CM.liftM2 (+) (eval a) (eval b)
eval (Var x)        = lookupVar x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  extendEnv x v1 (eval e2)
eval (NewRef e)     = newRef =<< eval e    -- new
eval (Deref e)      = deref  =<< eval e    -- new
eval (pe := ve)     = do                   -- new
  p <- eval pe
  v <- eval ve
  p =: v

-- * Utilities: testing and parsing

testExercise = parse "p:=0"
testUgly = parse "let p=new 1; let q=new 1738; !(p+1)"
test = runEval $ eval testUgly

-- | The parser is parameterised over the abstract syntax.
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = \_ _ -> Var "language: not implemented: catch"
  }

parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
