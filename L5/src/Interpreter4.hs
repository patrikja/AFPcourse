{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Version 4 of the interpreter
module Interpreter4 where

import qualified Control.Applicative    as CA (Applicative(..))
import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS
import qualified Control.Monad.Error    as CME

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Parser as P(parseExpr, Language(..))

-- Same datatype - this version is only varying the monad transformers
data Expr = Lit Integer
          | Expr :+ Expr
          | Var Name
          | Let Name Expr Expr
          | NewRef Expr
          | Deref Expr
          | Expr := Expr
          | Catch Expr Expr
  deriving (Show)

type Name  = String
type Value = Integer
type Ptr   = Value
  -- dangerous language: any value can be used as a pointer

-- | An environment maps variables to values.
type Env = Map Name Value

emptyEnv :: Env
emptyEnv = Map.empty

-- | We need to keep track of the store containing the values of our
--   references. We also remember the next unused pointer.
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

-- | ...and explain how to turn strings into exceptions. This is used
--   for the fail operation in the monad.
instance CME.Error Err where
  strMsg = OtherError
  noMsg  = CME.strMsg ""

{-| We add an error monad to our evaluation monad. It matters whether
    we stick the error monad on the outside or the inside of the state
    monad. In this case we stick it on the inside.

    We can understand the interaction between the state monad and the
    error monad by looking at their implementations. With ErrorT on
    the outside, we will represent computations as

      ms (Either err a)   ~=   s -> (Either err a,  s)

    where ms is the underlying monad with state. Since the state is
    hidden inside m it is not affected by whether we return @Right a@
    or @Left err@.  In other words state changes won't be rolled back
    when there's an exception.

    If we turn it around, adding a state monad on top of an error
    monad, computations will be represented as

      s -> me (a, s)      ~=   s -> Either err  (a, s)

    Here it's clear that if a computation fails, we lose any changes
    to the state made by the failing computation since the state is
    just passed as a return value in the underlying monad.
-}

newtype Eval1 a = Eval1{ unEval1:: CMS.StateT Store   -- S.R.E$I
                                     (CMR.ReaderT Env
                                       (CME.ErrorT Err
                                         CMI.Identity))
                                           a }
  deriving (Functor, CA.Applicative,
            Monad, CMS.MonadState  Store,
                   CMR.MonadReader Env,
                   CME.MonadError  Err)

newtype Eval2 a = Eval2{ unEval2:: CME.ErrorT Err     -- E.S.R$I
                                     (CMS.StateT Store
                                       (CMR.ReaderT Env
                                         CMI.Identity))
                                           a }
  deriving (Functor, CA.Applicative,
            Monad, CMS.MonadState  Store,
                   CMR.MonadReader Env,
                   CME.MonadError  Err)

runEval1 :: Eval1 a -> Either Err a
runEval1 = CMI.runIdentity
         . CME.runErrorT
         . startReaderFrom emptyEnv
         . startStateFrom  emptyStore
         . unEval1

runEval2 :: Eval2 a -> Either Err a
runEval2 = CMI.runIdentity
         . startReaderFrom emptyEnv
         . startStateFrom  emptyStore
         . CME.runErrorT
         . unEval2

startStateFrom :: Monad m => state -> CMS.StateT state m a -> m a
startStateFrom = flip CMS.evalStateT
  -- CMS.evalStateT :: Monad m => CMS.StateT state m a ->
  --                                (state -> m a)

startReaderFrom :: env -> CMR.ReaderT env m a -> m a
startReaderFrom = flip CMR.runReaderT
  -- CMR.runReaderT :: CMR.ReaderT env m a ->
  --                              (env -> m a)


-- * Environment stuff

-- | Here we just remove the type annotation
-- lookupVar :: Name -> Eval Value
lookupVar x = do
  env <- CMR.ask
  case Map.lookup x env of
    Nothing -> CME.throwError $ UnboundVariable x
    Just v  -> return v

-- extendEnv :: Name -> Value -> Eval a -> Eval a
extendEnv x v = CMR.local (Map.insert x v)

-- * Store stuff

-- | Create a new reference containing the given value.
-- newRef :: Value -> Eval Ptr
newRef v = do
  s <- CMS.get
  let ptr = nextPtr s
      s'  = s { nextPtr = ptr + 1
              , heap    = Map.insert ptr v (heap s)
              }
  CMS.put s'
  return ptr

-- | This has also been changed to throw an error.
-- deref :: Ptr -> Eval Value
deref p = do
  h <- CMS.gets heap
  case Map.lookup p h of
    Nothing -> CME.throwError SegmentationFault
    Just v  -> return v

-- (=:) :: Ptr -> Value -> Eval Value
p =: v = do
  CMS.modify $ \s -> s { heap = Map.adjust (const v) p (heap s) }
  return v

-- | The case for 'Catch' simply uses the 'catchError' function from
--   the error monad.
-- eval :: Expr -> Eval Value
eval (Lit n)       = return n
eval (a :+ b)      = CM.liftM2 (+) (eval a) (eval b)
eval (Var x)       = lookupVar x
eval (Let x e1 e2) = do
  v1 <- eval e1
  extendEnv x v1 (eval e2)
eval (NewRef e)    = newRef =<< eval e
eval (Deref e)     = deref =<< eval e
eval (pe := ve)    = do
  p <- eval pe
  v <- eval ve
  p =: v
eval (Catch e1 e2) = eval e1 `CME.catchError` \_ -> eval e2

-- | Examples

testExpr1 = parse "!p+1738"
testExpr2 = parse "(try !p catch 0)+1738"
test1 = runEval1 $ eval testExpr1
test2 = runEval2 $ eval testExpr2
testExpr3 = parse "let one = new 1; \
                 \ let dummy = (try ((one := 2) + !7) catch 0); \
                 \ !one"
-- value is 1 if state is discarded when error occurs
-- value is 2 if state is preserved up to the error
test31 = runEval1 $ eval testExpr3
test32 = runEval2 $ eval testExpr3

-- Exercise: use QuickCheck to find a minimal example for which
--   runEval1 and runEval2 are different.

----------------
-- | Parser stuff.
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

parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
