{-# LANGUAGE GADTs #-}
module Typed where

-- | The type of well-typed expressions. There is no way to
-- construct an ill-typed expression in this datatype.
data Expr a where
  Lit   :: a                               -> Expr a
  (:+)  ::              Expr Int-> Expr Int-> Expr Int
--Plus  :: Num a =>     Expr a  -> Expr a  -> Expr a  
  (:==) :: Eq a =>      Expr a  -> Expr a  -> Expr Bool
  If    :: Expr Bool -> Expr a  -> Expr a  -> Expr a

-- | A type-safe evaluator.
eval :: Expr t -> t
eval (Lit x)       = x
eval (e1 :+ e2)    = (eval e1) + (eval e2)
eval (e1 :== e2)   = (eval e1) == (eval e2)
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3


eOK :: Expr Int
eOK  = If (Lit False) (Lit 1) (Lit 2 :+ Lit 1736)
-- eBad = If (LitB False) (LitN 1) (LitN 2 :+ LitB True)


{-
-- Alternative: create two datatypes ExprI and ExprB
data ExprB  where
  LitB  :: Bool                            -> ExprB
  EqB   ::              ExprB   -> ExprB   -> ExprB
  EqI   ::              ExprI   -> ExprI   -> ExprB
  IfB   :: ExprB     -> ExprB   -> ExprB   -> ExprB

data ExprI  where
  LitN  :: Int                             -> ExprI
  (:+)  ::              ExprI   -> ExprI   -> ExprI
  IfI   :: ExprB     -> ExprI   -> ExprI   -> ExprI
-}


-- parse :: String -> [(Expr a, String)]

inject :: Expr.Expr -> Typed.Expr a





{-
{-# ExistentialQuantification #-}
import qualified Expr as E
import Maybe (fromJust)


-- | We can forget that an expression is typed. For instance to
-- be able to reuse the pretty printer we already have.
forget :: Expr t -> E.Expr
forget e = case e of
  LitN n      -> E.LitN n
  LitB b      -> E.LitB b
  e1 :+ e2    -> forget e1  E.:+   forget e2
  e1 :== e2   -> forget e1  E.:==  forget e2
  If e1 e2 e3 -> E.If (forget e1) (forget e2) (forget e3)

instance Show (Expr t) where
  showsPrec p e = showsPrec p (forget e)

-- How to go the other way, turning an untyped expression into a
-- typed expression?

-- Answer: we have to do type checking! Moreover, our type
-- checker will have to convince the Haskell type checker to
-- allow us to construct an element of Expr t from an untyped
-- expression passing our type checker. In other words we are
-- not writing a type checker for our own benefit, but to
-- explain to GHC's type checker why a particular untyped term
-- is really well-typed.

-- | The types that an expression can have. Indexed by the
-- corresponding Haskell type.
data Type t where
  TInt  :: Type Int
  TBool :: Type Bool

instance Show (Type t) where
  show TInt  = "Int"
  show TBool = "Bool"

-- | Well-typed expressions of some type are just pairs of
-- expressions and types which agree on the Haskell type. The
-- /forall/ builds an existential type (exercise: think about
-- whether this makes sense).
data TypedExpr = forall t. Eq t =>   Expr t ::: Type t

instance Show TypedExpr where
  show (e ::: t) = show e ++ " :: " ++ show t

-- | When comparing two types it's not enough to just return a
-- boolean.  Remember that we're trying to convince GHC's type
-- checker that two types are equal, and just evaluating some
-- arbitrary function to True isn't going to impress it.
--
--   Instead we define a type of proofs that two types @a@ and
-- @b@ are equal.  The only way to prove two types equal is if
-- they are in fact the same, and then the proof is Refl.
-- Evaluating one of these proofs to 'Refl' will convince GHC's
-- type checker that the two type arguments are indeed equal
-- (how else could the proof be Refl?).
data Equal a b where
  Refl :: Equal a a

-- | The type comparison function returns a proof that the types
-- we compare are equal in the cases that they are.
(=?=) :: Type s -> Type t -> Maybe (Equal s t)
TInt  =?= TInt  = Just Refl
TBool =?= TBool = Just Refl
_     =?= _     = Nothing

-- | Finally the type inference algorithm. We're making heavy
-- use of the fact that pattern matching on a @Type t@ or an
-- @Equal s t@ will tell GHC's type checker interesting things
-- about @s@ and @t@.
infer :: E.Expr -> Maybe TypedExpr
infer e = case e of
  E.LitN n -> return (LitN n ::: TInt)

  E.LitB b -> return (LitB b ::: TBool)

  r1 E.:+ r2 -> do
    e1 ::: TInt  <-  infer r1
    e2 ::: TInt  <-  infer r2
    return (e1 :+ e2 ::: TInt)

  r1 E.:== r2 -> do
    e1 ::: t1    <-  infer r1
    e2 ::: t2    <-  infer r2
    Refl         <-  t1 =?= t2
    return (e1 :== e2 ::: TBool)

  E.If r1 r2 r3 -> do
    e1 ::: TBool <-  infer r1
    e2 ::: t2    <-  infer r2
    e3 ::: t3    <-  infer r3
    Refl         <-  t2 =?= t3
    return (If e1 e2 e3 ::: t2)

-- | We can do type checking by inferring a type and comparing
-- it to the type we expect.
check :: E.Expr -> Type t -> Maybe (Expr t)
check r t = do
  e ::: t' <- infer r
  Refl     <- t' =?= t
  return e

test1R = read "1+2 == 3"
test1  = Maybe.fromJust (infer test1R)

infix  0 :::
-}
infixl 6 :+
infix  4 :==


