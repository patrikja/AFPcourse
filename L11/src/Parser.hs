{-# LANGUAGE GADTs #-}

module Parser
  ( P, parse
  , symbol, pfail, (+++)
  , this, sat, chainLeft
  ) where

import Control.Applicative
import Control.Monad

type ParseResult s a = [(a, [s])]

data P s a where
  Fail   :: P s a
  -- ReturnChoice x p = return x +++ p
  ReturnChoice :: a -> P s a -> P s a
  -- SymbolBind f = symbol >>= f
  SymbolBind :: (s -> P s a) -> P s a

symbol = SymbolBind return
pfail  = Fail

SymbolBind f +++ SymbolBind g = SymbolBind (\x -> f x +++ g x)
Fail +++ q  = q
p +++ Fail  = p
ReturnChoice x p +++ q = ReturnChoice x (p +++ q)
p +++ ReturnChoice x q = ReturnChoice x (p +++ q)

instance Monad (P s) where
  return x = ReturnChoice x pfail
  Fail             >>= f = Fail
  ReturnChoice x p >>= f = f x +++ (p >>= f)
  SymbolBind k     >>= f = SymbolBind (k >=> f)

instance Functor (P s) where
  fmap = liftM

instance Applicative (P s) where
  pure = return
  (<*>) = ap

instance Alternative (P s) where
  empty = pfail
  (<|>) = (+++)

parse :: P s a -> [s] -> ParseResult s a
parse (SymbolBind f) (c : s) = parse (f c) s
parse (SymbolBind f) []      = []
parse Fail       _           = []
parse (ReturnChoice x p) s   = (x, s) : parse p s

-- Derived combinators

sat :: (s -> Bool) -> P s s
sat p = do
  t <- symbol
  if p t then return t
         else pfail

this :: Eq s => s -> P s s
this x = sat (x ==)

chainLeft :: P s (a -> a -> a) -> P s a -> P s a
chainLeft op term = do
    e <- term
    chain e
  where
    chain e = return e +++ do
      o  <- op
      e' <- term
      chain (e `o` e')

