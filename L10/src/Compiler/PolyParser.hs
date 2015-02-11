module Compiler.PolyParser where
-- This parser library has at least one bug planted.

type Parser s a = [s] -> [(a,[s])]

succeed :: a -> Parser s a
succeed v inp = [(v,inp)]

satisfy :: (s -> Bool) -> Parser s s
satisfy _p []     = []
satisfy  p (x:xs) = if p x then [(x,xs)] else []

lit :: Eq s => s -> Parser s s
lit x = satisfy (x==)

infixl 9 ..., *.., ..*
infix  8 `using`
infixr 7 |||

(|||) :: Parser s a -> Parser s a -> Parser s a
(|||) p1 p2 inp  = p1 inp ++ p2 inp

(...) :: Parser s a -> Parser s b -> Parser s (a,b)
(...) p1 p2 inp = concatMap f1 (p1 inp)
                  where
                  f1 (v1,inp1) =  map f2 (p2 inp1)
                                  where
                                  f2 (v2,inp2) = ((v1,v2),inp2)

(..*) :: Parser s a -> Parser s b -> Parser s a
p1 ..* p2 = (p1 ... p2) `using` fst

(*..) :: Parser s a -> Parser s b -> Parser s b
p1 *.. p2 = (p1 ... p2) `using` snd

infix `opt`

opt :: Parser s a -> a -> Parser s a
opt p v inp = [head ((p ||| succeed v) inp)]

using :: Parser s a -> (a->b) -> Parser s b
using p f inp = map (\(v,out) -> (f v,out)) (p inp)

many, some :: Parser s a -> Parser s [a]
many p = ((p ... many p) `using` cons) `opt` []
some p = (p ... many p) `using` cons

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs

the :: [(a,String)] -> a
the ((x,""):_) = x
the (_:rest)   = the rest
the _          = error "Parser s.hs: the: Parse error"

-- --------------------------------------------------------------

-- Adapting to the parser interface from lecture 4
-- P, symbol, (+++), pfail, parse, return, (>>=)

newtype P s a = P {unP :: Parser s a}

symbol :: P s s
symbol = P $ satisfy (const True)

(+++) :: P s a -> P s a -> P s a
P p +++ P q = P (p ||| q)

pfail :: P s a
pfail = P $ \_inp -> []

parse :: P s a -> [s] -> [(a,[s])]
parse = unP

instance Monad (P s) where
  return     =  P . succeed
  P p >>= f  =  P $ \inp -> do (x, rest) <- p inp
                               unP (f x) rest

-- Possible bug to use for debugging:
--                               (y, _   ) <- unP (f x) rest
--                               return (y, rest)
