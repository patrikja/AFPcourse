{-# LANGUAGE GADTs #-}

-- | Removing the constructor for (>>=).
module Parser2
  ( P           -- :: * -> * -> *
  , symbol      -- :: P s s
  , pfail       -- :: P s a
  , (+++)       -- :: P s a -> P s a -> P s a
  , ParseResult -- ParseResult s a = [(a, [s])]
  , parse       -- :: P s a -> [s] -> ParseResult s a
  ) where
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad       ((>=>))

type ParseResult s a = [(a, [s])]

{-| The first problem we identified was the list comprehension.
    The problem would go away if we could get rid of the
    constructor for (>>=). (We linearize the sequencing
    operation).  To do this, let's look at what laws we have
    about (>>=):

    L1.  return x  >>= f  ==  f x
    L3.  (p >>= f) >>= g  ==  p >>= (f >=> g)
    L4.  fail      >>= f  ==  fail
    L5.  (p +++ q) >>= f  ==  (p >>= f) +++ (q >>= f)

  We can simplify uses of (>>=) for all possible values of the
  first argument except 'symbol'. So what we do is to introduce
  a special constructor for this case and get rid of the general
  (:>>=).

      SymbolBind f  ==  symbol >>= f

  Observe that we can now remove the Symbol constructor as well
  since we have

      symbol  ==  symbol >>= return   {- by L2 -}
              ==  SymbolBind Return
-}
data P s a where
  Fail       :: P s a
  (:+++)     :: P s a -> P s a -> P s a
  Return     :: a -> P s a
  SymbolBind :: (s -> P s a) -> P s a 
                -- SymbolBind f  =  symbol >>= f

symbol :: P s s
symbol = SymbolBind Return

pfail :: P s a
pfail  = Fail

(+++) :: P s a -> P s a -> P s a
(+++)  = (:+++)

-- Using the laws we can calculate the definition of a "smart
-- constructor" bind to replace (:>>=):
bind :: P s a -> (a -> P s b) -> P s b
bind Fail           _f  =  Fail                         -- by L4
bind (Return x)      f  =  f x                          -- by L1
bind (p :+++ q)      f  =  (p >>= f) +++ (q >>= f)      -- by L5
bind (SymbolBind k)  f  =  SymbolBind (k >=> f) 
                           -- by L3 and def. of SymbolBind

instance Monad (P s) where
  return  =  Return
  (>>=)   =  bind

{-| Similarly we can calculate the definition of the run
  function for the new constructor by using the old run
  function:

      parse (SymbolBind f)  (c:s)               ==
      parse (Symbol :>>= f) (c:s)               ==
      [ (y, s'') | (x, s')  <- parse Symbol (c:s)
                 , (y, s'') <- parse (f x) s'
      ]                                         ==
      [ (y, s'') | (x, s')  <- [(c, s)]
                 , (y, s'') <- parse (f x) s' ] ==
      [ (y, s'') | (y, s'') <- parse (f c) s ]  ==
      parse (f c) s
-}
parse :: P s a -> [s] -> ParseResult s a
parse (SymbolBind f)  (c : s)  =  parse (f c) s
parse (SymbolBind _)  []       =  []
parse Fail            _        =  []
parse (p :+++ q)      s        =  parse p s ++ parse q s
parse (Return x)      s        =  [(x, s)]

-- Now the list comprehension (concatMap) is gone. Next up is
-- the list concatenation in the (:+++) case.

--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor (P s) where
  fmap = liftM
  
instance Applicative (P s) where
  pure   = return
  (<*>)  = ap
