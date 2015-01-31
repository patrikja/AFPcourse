-- | The naive shallow embedding following the semantics, using 
--   lists to implement the multisets (also called bags).
module Parser0
  ( P           -- :: * -> * -> *
  , symbol      -- :: P s s
  , pfail       -- :: P s a
  , (+++)       -- :: P s a -> P s a -> P s a
  , PSem        -- :: * -> * -> *
  , parse       -- :: P s a -> PSem s a
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

{-

The semantics of a parser of type 'P s a' is a function from a
string of 's' to a multiset of results paired with the remaining 
parts of the input string. We use a multiset to capture the fact
that we don't care about the order of the results.

The semantic function [| _ |], also called run, is defined as follows
(we use {| |} to denote multisets and \/ for multiset union).

[| _ |] :: P s a -> [s] -> {| (a, [s]) |}
[| symbol   |] (c : s) = {| (c, s) |}
[| symbol   |] []      = {| |}
[| pfail    |] s       = {| |}
[| p +++ q  |] s       = [| p |] s  \/  [| q |] s
[| return x |] s       = {| (x, s) |}
[| p >>= f  |] s       = {| (y, s'') | (x, s')  <- [| p   |] s
                                     , (y, s'') <- [| f x |] s'
                         |}

The RHS of the definition of bind can also be written 
  concatMap (uncurry (run . f)) (run p s)
which is more practical to calulate with.

Using this semantics we can prove (exercise) a number of useful
laws about parsers. We will use these laws later to derive an
efficient implementation of the library.

For two parsers p and q we define

  p == q  iff  forall s. [| p |] s == [| q |] s

Monad Laws

  L1.  return x >>= f   ==  f x
  L2.  p >>= return     ==  p
  L3.  (p >>= f) >>= g  ==  p >>= (\x -> f x >>= g)

More laws about >>=

  L4.  fail >>= f       ==  fail
  L5.  (p +++ q) >>= f  ==  (p >>= f) +++ (q >>= f)

Laws about (+++) and fail

  L6.  fail +++ q       ==  q
  L7.  p +++ fail       ==  p

Laws about (+++)

  L8.  (p +++ q) +++ r  ==  p +++ (q +++ r)
  L9.  p +++ q          ==  q +++ p   -- bag => order irrelevant

Laws about symbol

  L10. (symbol >>= f) +++ (symbol >>= g)
                        ==  symbol >>= (\c -> f c +++ g c)

Here is the proof of L10 for the case of a non-empty input
string:

  [| (symbol >>= f) +++ (symbol >>= g) |] (c:s)         
==  { Def. of [| p +++ q |] }
  [| symbol >>= f |] (c:s)  \/  [| symbol >>= g |] (c:s)  
==  { Def. of [| p >>= f |] and [| symbol |] }
  [| f c |] s  \/  [| g c |] s                            
==  { Def. of [| p +++ q |] "backwards" }
  [| f c +++ g c |] s                                   
==  { Def. of [| p >>= f |] and [| symbol |] "backwards" }
  [| symbol >>= (\c -> f c +++ g c) |] (c:s)

We can make a shallow embedding following the semantics, using
lists for the multisets.

-}

type ParseResult s a  =  [(a, [s])]
type PSem s a  =  [s] -> ParseResult s a

-- | We can implement parsers by their semantics (shallow embedding).
newtype P s a = P (PSem s a)

-- | The 'parse' function is trivial.
parse :: P s a -> PSem s a
parse (P p)  =  p

-- The operations simply follow the semantics inserting the
-- newtype constructor where appropriate.

symbol :: P s s
symbol = P symbolP 

symbolP :: PSem s s -- [s]   ->  ParseResult s s
symbolP  (c : s)  =  [(c, s)]
symbolP  []       =  []

pfail :: P s a
pfail = P $ \_ -> []

(+++) :: P s a -> P s a -> P s a
P p  +++  P q  =  P (mplusP p q)

mplusP :: PSem s a -> PSem s a -> PSem s a
mplusP p q  =  \s ->  p s ++ q s

returnP :: a -> PSem s a 
returnP x = \s -> [(x, s)]

bind :: P s a -> (a -> P s b) -> P s b
bind p f  =  P $ \s -> [ (y, s'') 
                       | (x, s')  <- parse p s
                       , (y, s'') <- parse (f x) s'
                       ]
-- or
bind' :: P s a -> (a -> P s b) -> P s b
bind' p k  =  P $ bindP (parse p) (parse . k)

bindP :: PSem s a -> (a -> PSem s b) -> PSem s b
bindP p f = concatMap (uncurry f) . p

instance Monad (P s) where
  return x  =  P $ returnP x
  p >>= f   =  bind p f
  fail _    =  pfail


-- There are a number of efficiency problems with this
-- implementation.  To solve them we'll start with a naive deep
-- embedding and successively refine it to get something
-- efficient.

--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor (P s) where
  fmap = liftM
  
instance Applicative (P s) where
  pure   = return
  (<*>)  = ap
