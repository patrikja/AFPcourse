{-# LANGUAGE GADTs #-}

-- | Naive deep embedding of the parsing library.
module Parser1
  ( P           -- :: * -> * -> *
  , symbol      -- :: P s s
  , pfail       -- :: P s a
  , (+++)       -- :: P s a -> P s a -> P s a
  , ParseResult -- ParseResult s a = [(a, [s])]
  , parse       -- :: P s a -> [s] -> ParseResult s a
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type ParseResult s a = [(a, [s])]
type PSem s a  =  [s] -> ParseResult s a

-- | Each operation is turned into a constructor.
data P s a where
  Symbol :: P s s
  Fail   :: P s a
  (:+++) :: P s a -> P s a -> P s a
  Return :: a -> P s a
  (:>>=) :: P s a -> (a -> P s b) -> P s b

symbol :: P s s
symbol = Symbol

pfail :: P s a
pfail  = Fail

(+++) :: P s a -> P s a -> P s a
(+++)  = (:+++)

instance Monad (P s) where
  return = Return
  (>>=)  = (:>>=)

-- | The parse function corresponds exactly to the semantics,
--   except that we use lists instead of multisets.
parse :: P s a -> PSem s a
parse Symbol      (c : s)  =  [(c, s)]
parse Symbol      []       =  []
parse Fail        _        =  []
parse (p :+++ q)  s        =  parse p s ++ parse q s
parse (Return x)  s        =  [ (x, s) ]
parse (p :>>= f)  s        =  bindS (parse p) (parse . f) s


{- Here we have shortened the list comprehension from the
   semantics in Parser0.hs to simplify program calculation:

  [ (y, s'') | (x, s')  <- parse p s
             , (y, s'') <- parse (f x) s']
= { let q = parse p; g = parse . f; ys = (y, s'') }
  [ ys | (x, s')  <- q s
       , ys       <- g x s']
= { List comprehension translation }
  concatMap (\(x, s') -> g x s') (q s)
= { def. uncurry }
  concatMap (uncurry g) (q s)
= { use this to define bindS }
  (q `bindS` g) s
-}

bindS :: PSem s a -> (a -> PSem s b) -> PSem s b
q `bindS` g = concatMap (uncurry g) . q


{- There are three sources of inefficiency that we can identify:

   1. The list comprehension builds a lot of intermediate lists
      which might be costly. (Partly fixed already.)

   2. List append (++) is linear in its first argument which
      means that left nested applications of (+++) get a
      quadratic behaviour.

   3. (+++) is treated in a depth first way, first computing the
      results of the left parser, then computing the results of
      the second parser. This leads to a space leak since we
      have to hang on to the input string to feed to the second
      parser, while traversing the string with the first parser.

  To fix these problems we'll use the laws stated in Parser0 to
  simplify our implementation.

-}

--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor (P s) where
  fmap = liftM
  
instance Applicative (P s) where
  pure   = return
  (<*>)  = ap
