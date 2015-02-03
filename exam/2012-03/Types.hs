{-# LANGUAGE GADTs, TypeFamilies #-}
module Types where
data Zero
data Suc n

data Vec a n where
  Nil   :: Vec a Zero
  Cons  :: a -> Vec a n -> Vec a (Suc n)

type family    Add  m        n  :: *
type instance  Add  Zero     n  =  n     
type instance  Add  (Suc m)  n  =  Suc (Add m n)

{- Task (a): Give the signature and implementation of |(++)| for
vector concatenation and explain why it type checks.  -}

(+++) :: Vec a m -> Vec a n -> Vec a (Add m n)
Nil        +++  ys  =  ys
Cons x xs  +++  ys  =  Cons x (xs +++ ys)
-- Nil :: Vec a Zero
-- return type = Vec a (Add Zero n) = Vec a n = the type of ys
-- Cons x xs :: Vec a (Suc m)
-- return type = Vec a (Add (Suc m) n) = Vec a (Suc (Add m n)) = type of the RHS

{- Would it still type check with the alternative definition of
type-level addition below?  Why/why not?  
-}

type family    Add'  m  n        :: *
type instance  Add'  m  Zero     =  m     
type instance  Add'  m  (Suc n)  =  Suc (Add' m n)

{- No, it would not type check. The case split in the function
definition must match the case split in the type family, otherwise the
type unification will fail. It is possible to work around it using a
family for type equality and a coerce function. -}

-- Task (b): Implement a GADT |Fin n| for unary numbers below |n| and
-- a lookup function |(!) :: Vec a n -> Fin n -> a|.

data Fin n where
  Fzero :: Fin (Suc n)
  Fsuc  :: Fin n -> Fin (Suc n)
  
(!) :: Vec a n -> (Fin n -> a)
Cons x xs  !  Fzero    =  x
Cons x xs  !  (Fsuc i) =  xs ! i

{- Task (c): Briefly explain the Curry-Howard correspondence for
``false'', ``true'', ``implies'', ``and'', ``or''.

----

  The Curry-Howard correspondence provides a "Logic" reading of types:
  
  Types             Logic
  p :: P            p is a proof of P

  empty type        False
  non-empty type    True
  P -> Q            P implies Q
  (P, Q)            P and Q
  Either P Q        P or Q
  
-- Not asked for in the exam:
  (a :: A) -> P     ∀a:A. P       -- dependent function type
  (a :: A, P)       ∃a:A. P       -- dependent pair type

-}

-- ................
-- Test code - not part of the exam

v1 = Cons 1 (Cons 7 Nil)
v2 = Cons 3 (Cons 8 Nil)
long = v1 +++ v2
two = Fsuc (Fsuc Fzero)
  
main = print (long ! two == 3)
