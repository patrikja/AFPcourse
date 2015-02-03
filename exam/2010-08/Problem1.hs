{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module Problem1 where
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Error as CME
import Test.QuickCheck
import Problem1.Types
-- Problem1: a) Define Calc and eval

-- This solution abstracts a bit from the exam question, generalising
-- the type in the Num constructor from Integer to a and from CalcM to
-- some MonadState. This is not needed to pass. (The monomorphic types
-- are shown in comments - they can be more intuitive than the
-- generalised polymorphic types.)

type Calc = C Value
data C a = CBin BOp (C a) (C a) 
         | CUn  UOp (C a) 
         | CNull NullOp 
         | Num a                    deriving (Eq, Show)
data BOp    = Add | Sub | Mul | Div deriving (Eq, Show)
data UOp    = Inv | Neg   | M | MP  deriving (Eq, Show)
data NullOp = MR | MC               deriving (Eq, Show)

-- eval :: Calc -> CalcM Value
eval :: (CMS.MonadState v m, Fractional v) => C v -> m v
eval (CBin op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalBOp op v1 v2

eval (CUn op e) = do
  v <- eval e
  evalUOp op v

eval (CNull op) = do
  evalNullOp op

eval (Num n)    = return n
--  return (fromInteger n) was used before the generalisation

-- evalBOp :: BOp -> Value -> Value -> CalcM Value
evalBOp :: (Fractional v, Monad m) => BOp -> (v->v->m v)
evalBOp Div v1 0 = fail "DivZ"
evalBOp op v1 v2 = return $ evalBOpPure op v1 v2

-- evalBOpPure :: BOp -> Value -> Value -> Value
evalBOpPure :: Fractional v => BOp -> (v->v->v)
evalBOpPure Div = (/)
evalBOpPure Mul = (*)
evalBOpPure Add = (+)
evalBOpPure Sub = (-)

----------------
-- evalUOp :: UOp -> v -> CM v v
evalUOp :: (Fractional v, CMS.MonadState v m) => UOp -> (v->m v)
evalUOp Inv 0 = fail "InvZ"
evalUOp M   v = do
  putMem v
evalUOp MP  v = do
  m <- getMem
  putMem (v+m)
evalUOp op  v = return $ evalUOpPure op v

-- evalUOpPure :: UOp -> Value -> Value
evalUOpPure :: Fractional v => UOp -> (v->v)
evalUOpPure Inv = (1/)
evalUOpPure Neg = negate

----------------

-- evalNullOp :: NullOp -> CalcM Value
evalNullOp :: (Num v, CMS.MonadState v m) => NullOp -> m v
evalNullOp MR = getMem
evalNullOp MC = putMem 0

----------------
-- getMem :: CalcM Value
getMem :: CMS.MonadState v m => m v
getMem    = CMS.get

-- putMem :: Value -> CalcM Value
putMem :: CMS.MonadState v m => v -> m v
putMem m = do
  CMS.put m
  return m

-- End of Problem1: a)

----------------------------------------------------------------
-- Problem1 b) Define CalcM + MonadState operations

-- Alt. b1) Use imported Control.Monad.State and Control.Monad.Error:
type CM v = CMS.StateT v (Either Err)
type CalcM = CMS.StateT Mem (Either Err)
-- All instances are automatic in this alternative

-- Alt. b2) Define your own: see Problem1.CalcMInstance

-- End of Problem1: b)

---------------------------------------------------------------
-- Problem1: c) Define the Monad laws as QC prop.s

----------------
-- Monad laws

-- Polymorphic:
leftId :: (Monad m, Eq (m b)) => a -> (a->m b) -> Bool
leftId a f   =  (return a >>= \x -> f x) == f a

rightId :: (Monad m, Eq (m a)) => m a -> Bool
rightId m    =  (m >>= \x-> return x) == m

assoc :: (Monad m, Eq (m c)) => m a -> (a->m b) -> (b-> m c) -> Bool
assoc m f g  =  ((m >>= f) >>= g)  ==   (m >>= (\x-> f x >>= g))

-- End of Problem1: c)

-- Problem1: d) running tests?

-- For CalcM:
leftIdCM ::  (Arbitrary a, Arbitrary (CalcM a), Eq (CalcM b)) => 
             a -> (a->CalcM b) -> Bool
leftIdCM  =  leftId

rightIdCM :: (Arbitrary (CalcM a), Eq (CalcM a)) => CalcM a -> Bool
rightIdCM  = rightId

assocCM ::   (Arbitrary a, Arbitrary (CalcM a), Arbitrary (CalcM b), Eq (CalcM c)) =>
             CalcM a -> (a->CalcM b) -> (b-> CalcM c) -> Bool
assocCM  =  assoc

{- You would also need to 
* fix monomorphic types for a, b, c
* define generators for CalcM a
* define equality checks for CalcM a values

Equality checking of arbitrary functions is undecidable. This can be
solved by letting QuickCheck generate random start-states and test
equality after running the CalcM monad.

-}

-- End of Problem1: d)
