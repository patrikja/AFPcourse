-- A DSL for symbolic algebra.
{-# LANGUAGE GADTs #-}
module DSL where
import Data.Maybe (isJust)
import Control.Monad(liftM, liftM2)
import Test.QuickCheck

{- The |Num|, |Fractional| and |Floating| classes in Haskell provide an
API for several mathematical operations and the standard library
provides instances for several base types like integers, floating
point numbers and rationals. Your task here is to implement a DSL for
symbolic expressions for the following subset of this API: -}

{- Task 2 (a) Implement a type |Sym v| as a deep embedding of the API
\& symbolic variables of type |v|.  -}

data Sym v where
  Var     :: v -> Sym v
  (:+)    :: Sym v -> Sym v -> Sym v
  (:*)    :: Sym v -> Sym v -> Sym v
  Negate  :: Sym v -> Sym v
  Lit     :: Integer -> Sym v
  (:/)    :: Sym v -> Sym v -> Sym v
  Pi      :: Sym v
  Exp     :: Sym v -> Sym v
  Log     :: Sym v -> Sym v
  Sin     :: Sym v -> Sym v
  Cos     :: Sym v -> Sym v
    deriving (Eq, Show)  -- not part of exam question

{- Task 2 (b) Implement parts of a run function |eval :: Floating n =>
(v -> Maybe n) -> Sym v -> Maybe n|. It is enough to implement the
cases for variables, |(+)|, |negate|, |fromInteger|, |(/)| and |exp|.
-}

eval :: Floating n => (v -> Maybe n) -> Sym v -> Maybe n
eval lookup = ev
  where  ev (Var v)     = lookup v
         ev (e1 :+ e2)  = liftM2  (+)     (ev e1)  (ev e2)
         ev (Negate e)  = liftM   negate  (ev e)
         ev (Lit i)     = return  (fromInteger i)
         ev (e1 :/ e2)  = liftM2  (/)     (ev e1)  (ev e2)
         ev (Exp e)     = liftM   exp     (ev e)
         -- end of required part
         ev (e1 :* e2)  = liftM2  (*)     (ev e1)  (ev e2)
         ev Pi          = return  pi
         ev (Log e)     = liftM   log     (ev e)
         ev (Sin e)     = liftM   sin     (ev e)
         ev (Cos e)     = liftM   cos     (ev e)


{- Task 2 (c): Implement an algebraic simplification function 
|simp :: Sym v -> Maybe (Sym v)| which handles the rules |0 * x == 0|,
|sin pi == 0|, |log (exp x) == x| and which fails (with |Nothing|) on
division by zero.  -}

simp :: Sym v -> Maybe (Sym v)
simp (e1 :* e2)  = do  e1' <- simp e1
                       e2' <- simp e2
                       return $ case e1' of
                         Lit 0  -> Lit 0
                         _      -> e1' :* e2'
simp (Sin e)     = do  e' <- simp e
                       return $ case e' of
                         Pi     -> Lit 0
                         _      -> Sin e'
simp (Log e)     = do  e' <- simp e
                       return $ case e' of
                         Exp e2 -> e2
                         _      -> Log e'
simp (e1 :/ e2)  = do  e1' <- simp e1
                       e2' <- simp e2
                       case e2' of
                         Lit 0  -> Nothing
                         _      -> Just $ e1' :/ e2'
-- If no rule applies, just simplify recursively:
simp (e1 :+ e2)  = liftM2  (:+)    (simp e1)  (simp e2)
simp (Negate e)  = liftM   Negate  (simp e)
simp (Exp e)     = liftM   Exp     (simp e)
simp (Cos e)     = liftM   Cos     (simp e)
-- Base cases: Sym, Lit, Pi
simp e           = return e


-- Not part of the exam question:
testrule1 :: Sym Char -> Property
testrule1 x  = isJust (simp x)  ==>  simp (Lit 0 :* x) == Just (Lit 0)
testrule2    = simp (Sin Pi :: Sym Char)  == Just (Lit 0)
testrule3 :: Sym Char -> Bool
testrule3 x  = simp (Log (Exp x))  == simp x

{- Task 2 (d):   
  Is there a reasonable |Monad| instance for |Sym|? If so, implement
  |return| and sketch |(>>=)|, otherwise explain why not.
-}

-- Yes, there is a reasonable Monad instance with (>>=) as substitution.

instance Monad Sym where
  return  = Var
  (>>=)   = bindSym
  
bindSym :: Sym a -> (a -> Sym b) -> Sym b
bindSym (Var v)     f  = f v
bindSym (e1 :+ e2)  f  = (bindSym e1 f) :+ (bindSym e2 f)
bindSym (Lit i)     f  = Lit i
-- ... always apply bindSym recursively, details not needed for the exam

----------------------------------------------------------------
-- Utilities (not part of exam question)
instance Arbitrary v => Arbitrary (Sym v) where
  arbitrary  = sized (arbitrarySym arbitrary)
--  shrink     = shrinkSym shrink 

arbitrarySym :: Gen v -> Int -> Gen (Sym v)  
arbitrarySym g n | n <= 0     = oneof [liftM Var g, liftM Lit arbitrary, return Pi]
                 | otherwise  = oneof [unary, binary]
  where unary  = do  unop <- elements [Negate, Exp, Log, Sin, Cos]
                     liftM unop (arbitrarySym g (n-1))
        binary = do  binop <- elements [(:+), (:*), (:/)]
                     let genHalfSize = arbitrarySym g (n `div` 2)
                     liftM2 binop genHalfSize genHalfSize

{-
shrinkSym :: (v -> [v]) -> (Sym v -> [Sym v])
shrinkSym shr = error "TBD"
-}
