{-# LANGUAGE GADTs #-}

-- | Removing the constructor for (+++)
module Parser3
  ( P           -- :: * -> * -> *
  , symbol      -- :: P s s
  , pfail       -- :: P s a
  , (+++)       -- :: P s a -> P s a -> P s a
  , ParseResult -- ParseResult s a = [(a, [s])]
  , parse       -- :: P s a -> [s] -> ParseResult s a
  ) where
import Control.Monad((>=>))

type ParseResult s a = [(a, [s])]

{-| The second problem was the use of (++) when parsing a
  choice. To fix this we'll remove the (:+++) constructor. (We
  linearize the choice operation). Let's look at the laws that
  might be useful:

    L6.  fail +++ q       ==  q
    L7.  p +++ fail       ==  p
    L8.  (p +++ q) +++ r  ==  p +++ (q +++ r)
    L9.  p +++ q          ==  q +++ p
    L10. (symbol >>= f) +++ (symbol >>= g)
                          ==  symbol >>= (\c -> f c +++ g c)

  We seem to have laws about (+++) for all possible arguments
  except return, hence we add a constructor for this special
  case and get rid of the general choice.

      ReturnChoice x p == return x +++ p

  Note that since (+++) is commutative we don't need a
  constructor for p +++ return x. Using multisets is paying off!

  Using L7 we can get rid of the Return constructor as well:

      return x  ==  return x +++ fail  ==  ReturnChoice Fail
-}
data P s a where
  Fail         ::                 P s a
  ReturnChoice :: a -> P s a   -> P s a 
    -- ReturnChoice x p == return x +++ p
  SymbolBind   :: (s -> P s a) -> P s a 
    -- SymbolBind f     == symbol >>= f

symbol :: P s s
symbol = SymbolBind return

pfail :: P s a
pfail  = Fail

{-| Using the laws listed above we can derive the implementation
  of (+++). Note the use of associativity and commutativity of
  (+++) in the last two clauses.

  In a stroke of luck, while removing the (:+++) constructor we
  also got rid of the space leak introduced by backtracking.
  Looking at the first clause, we see that this choice operator
  runs its arguments in parallel -- if both sides in a choice
  want to consume a symbol then we consume a symbol once and
  feed it to both parsers, rather than as before running the
  first parser to completion before feeding the second parser.

-}
(+++) :: P s a -> P s a -> P s a
SymbolBind f     +++ SymbolBind g     = SymbolBind 
                                          (\x -> f x +++ g x)
Fail             +++ q                = q
p                +++ Fail             = p
ReturnChoice x p +++ q                = ReturnChoice x (p +++ q)
p                +++ ReturnChoice x q = ReturnChoice x (p +++ q)

bind :: P s a -> (a -> P s b) -> P s b
bind Fail                f  =  Fail
bind (ReturnChoice x p)  f  =  f x +++ (p >>= f) -- see below
bind (SymbolBind k)      f  =  SymbolBind (k >=> f)

{-| Deriving the ReturnChoice case using L5 and L1:

  bind (ReturnChoice x p) f       ==  -- by def.
  bind (return x +++ p)   f       ==  -- L5
  (return x >>= f) +++ (p >>= f)  ==  -- L1
  f x +++ (p >>= f)

-}

instance Monad (P s) where
  return x  =  ReturnChoice x pfail
  (>>=)     =  bind

-- | Now the use of (++) has been replaced by a (:) and all our
-- problems have gone away!

parse :: P s a -> [s] -> ParseResult s a
parse (SymbolBind f)      (c : s)  =  parse (f c) s
parse (SymbolBind f)      []       =  []
parse Fail                _        =  []
parse (ReturnChoice x p)  s        =  (x, s) : parse p s

{- There is still one remaining source of inefficiency. If you
  look at the definition of (>>=), you'll see that it's linear
  in the size of its first argument. This means that we get a
  similar problem to the use of (++), namely a quadratic
  behaviour for left nested uses of (>>=). In order to fix this
  we cannot use the method we've been using so far, there is no
  constructor to remove to fix the problem. Instead we have to
  use another technique, called a "context passing"
  implementation. Read more about it in the paper.
-}
