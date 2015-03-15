{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Version 3 of the interpreter
module Interpreter3 where

import qualified Control.Applicative    as CA (Applicative(..))
import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS
import qualified Control.Monad.Error    as CME -- new

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Parser as P(parseExpr, Language(..))

-- | Finally, we add a way to catch exceptions arising from
-- unbound variables and dereferencing non-existent pointers.
data Expr = Lit Integer
          | Expr :+ Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr
          | Deref Expr
          | Expr := Expr
          | Catch Expr Expr      -- new
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

-- | We add an exception type...
data Err = SegmentationFault
         | UnboundVariable String
         | OtherError String
  deriving Show

-- | ...and explain how to turn strings into exceptions. This is
-- used for the fail operation in the monad.
instance CME.Error Err where
  strMsg = OtherError
  noMsg  = CME.strMsg ""

{-| We add an error monad to our evaluation monad. It matters
whether we stick the error monad on the outside or the inside of
the state monad. In this case we stick it on the inside.

We can understand the interaction between the state monad and
the error monad by looking at their implementations. With ErrorT
on the outside, we will represent computations as

  ms (Either err a)
which is roughly
  s -> (Either err a, s)

where ms is the underlying monad with state. Since the state is
hidden inside m it is not affected by whether we return @Right
a@ or @Left err@.  In other words state changes won't be rolled
back when there's an exception.

If we turn it around, adding a state monad on top of an error
monad, computations will be represented as

  s -> me (a, s)
which is roughly
  s -> Either e (a, s)

Here it's clear that if a computation fails, we lose any changes
to the state made by the failing computation since the state is
just passed as a return value in the underlying monad.  
-}

newtype Eval a = Eval { unEval :: CMS.StateT Store
                                    (CMR.ReaderT Env
                                      (CME.ErrorT Err  -- new
                                        CMI.Identity))
                                          a }
  deriving (Functor, CA.Applicative,
            Monad, CMS.MonadState  Store
                 , CMR.MonadReader Env
                 , CME.MonadError  Err -- new
                 ) 

runEval :: Eval a -> Either Err a
runEval = CMI.runIdentity
        . CME.runErrorT
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


-- * Environment stuff

-- | We've changed variable lookup to throw an error in our new
-- error type rather than using 'fail'.
lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- CMR.ask
  case Map.lookup x env of
    Nothing  -> CME.throwError (UnboundVariable x) -- new
    Just v   -> return v

extendEnv :: Name -> Value -> Eval a -> Eval a
extendEnv x v = CMR.local (Map.insert x v)

-- * Store stuff

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

-- | This has also been changed to throw an error.
deref :: Ptr -> Eval Value
deref p = do
  h <- CMS.gets heap
  case Map.lookup p h of
    Nothing -> CME.throwError SegmentationFault
    Just v  -> return v

(=:) :: Ptr -> Value -> Eval Value
p =: v = do
  CMS.modify $ \s -> s { heap = Map.adjust (const v) p (heap s) }
  return v

-- | The case for 'Catch' simply uses the 'catchError' function
-- from the error monad.
eval :: Expr -> Eval Value
eval (Lit n)        = return n
eval (a :+ b)       = CM.liftM2 (+) (eval a) (eval b)
eval (Var x)        = lookupVar x
eval (Let x e1 e2)  = do
  v1 <- eval e1
  extendEnv x v1 (eval e2)
eval (NewRef e)     = newRef =<< eval e
eval (Deref e)      = deref =<< eval e
eval (pe := ve)     = do
  p <- eval pe
  v <- eval ve
  p =: v
eval (Catch e1 e2)  = CME.catchError (eval e1) (\_err -> eval e2)
--  catchError :: Eval Value -> (e -> Eval Value) -> Eval Value
                         
                         
                      

-- * Examples

testExpr1, testExpr2 :: Expr
testExpr1 = parse "let p=0; !p+1738"
testExpr2 = parse "(try !p catch 0)+1738"
testExpr3 :: Expr
testExpr3 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0); \
                 \ !one"
-- | value is 1 if state is discarded when error occurs
--   value is 2 if state is preserved up to the error
test1 :: Either Err Value
test1 = runEval $ eval testExpr1 
test2 :: Either Err Value
test2 = runEval $ eval testExpr2 
test3 :: Either Err Value
test3 = runEval $ eval testExpr3






----------------
-- | Parser stuff.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = Catch
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
