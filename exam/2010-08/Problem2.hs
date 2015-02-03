{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Typed where
import qualified Expr as E
import Maybe (fromJust)

data Expr t where
  Lit   :: (Eq t, Show t) => t                     -> Expr t
  (:+)  ::                   Expr Int -> Expr Int  -> Expr Int
  (:==) :: (Eq t, Show t) => Expr t   -> Expr t    -> Expr Bool
  If    ::      Expr Bool -> Expr t   -> Expr t    -> Expr t    -- ADDED a)

eval :: Expr t -> t
eval (Lit x)       = x
eval (e1 :+ e2)    = eval e1 + eval e2
eval (e1 :== e2)   = eval e1 == eval e2
eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3       -- ADDED b)

eOK :: Expr Int
eOK  = If (Lit False) (Lit 1) (Lit 2 :+ Lit 1736)

data Type t where
  TInt  :: Type Int
  TBool :: Type Bool

data TypedExpr = forall t. (Eq t, Show t) =>   Expr t ::: Type t

data Equal a b where
  Refl :: Equal a a

-- | The type comparison function returns a proof that the types we
--   compare are equal in the cases that they are.
(=?=) :: Type s -> Type t -> Maybe (Equal s t)
TInt  =?= TInt  = Just Refl
TBool =?= TBool = Just Refl
_     =?= _     = Nothing

infer :: E.Expr -> Maybe TypedExpr
infer e = case e of
  E.LitN n -> return (Lit n ::: TInt)

  E.LitB b -> return (Lit b ::: TBool)

  r1 E.:+ r2 -> do
    e1 ::: TInt  <-  infer r1
    e2 ::: TInt  <-  infer r2
    return (e1 :+ e2 ::: TInt)

  r1 E.:== r2 -> do
    e1 ::: t1    <-  infer r1
    e2 ::: t2    <-  infer r2
    Refl         <-  t1 =?= t2
    return (e1 :== e2 ::: TBool)

  E.If r1 r2 r3 -> do                                           -- ADDED c)
    e1 ::: TBool <-  infer r1
    e2 ::: t2    <-  infer r2
    e3 ::: t3    <-  infer r3
    Refl         <-  t2 =?= t3
    return (If e1 e2 e3 ::: t2)

check :: E.Expr -> Type t -> Maybe (Expr t)
check r t = do
  e ::: t' <- infer r
  Refl     <- t' =?= t
  return e

test1R = read "1+2 == 3"
test1  = Maybe.fromJust (infer test1R)

infixl 6 :+
infix  4 :==
infix  0 :::

instance Show (Type t) where
  show TInt  = "Int"
  show TBool = "Bool"
instance Show TypedExpr where
  show (e ::: t) = show e ++ " :: " ++ show t

instance Show t => Show (Expr t) where
  showsPrec = showsPrecExpr

showsPrecExpr :: Show t => Int -> Expr t -> ShowS               -- ADDED d)
showsPrecExpr p e = case e of
    Lit n       -> shows n

    e1 :+ e2     -> showParen (p > 2) $
      showsPrec 2 e1 . showString " + " . showsPrec 3 e2

    e1 :== e2    -> showParen (p > 1) $
      showsPrec 2 e1 . showString " == " . showsPrec 2 e2

    If e1 e2 e3  -> showParen (p > 0) $                         -- ADDED d)
      showString "if "    . shows e1 .
      showString " then " . shows e2 .
      showString " else " . shows e3
