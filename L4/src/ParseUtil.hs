module ParseUtil (module Parsers, module ParseUtil) where
import Data.Char(isDigit)
import Parsers -- (P, symbol, pfail, (+++), parse)
-- import SomeP as Parsers -- you can try with different versions

----------------------------------------------------------------
-- Interface:

-- | Parse a symbol satisfying a given predicate.
sat :: (s -> Bool) -> P s s

-- | Parse a particular symbol.
this :: Eq s => s -> P s s

-- | Parse a digit as a number.
digitP :: P Char Int

-- | Parse a left associative operator carefully avoiding left
-- recursion.
--    chainLeft Op T ::= E
--          where E  ::=  E Op T  |  T
chainLeft :: P s (a -> a -> a)  ->  P s a  ->  P s a

----------------------------------------------------------------
-- Implementation:
sat p = do
  x <- symbol
  if p x  then  return x
          else  pfail

this x = sat (x ==)

digitP = do
    c <- sat isDigit
    return (charToInt c)
  where
    charToInt c = fromEnum c - fromEnum '0'

chainLeft op term = do
    e <- term
    chain e
  where
    chain e = return e +++ do
      o  <- op
      e' <- term
      chain (e `o` e')

