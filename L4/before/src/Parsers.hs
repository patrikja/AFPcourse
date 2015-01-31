{-# LANGUAGE GADTs #-}
module Parsers
  ( P           -- :: * -> * -> *
  , symbol      -- :: P s s
  , pfail       -- :: P s a
  , (+++)       -- :: P s a -> P s a -> P s a
  , Semantics   -- :: * -> * -> *
  , parse       -- :: P s a -> Semantics s a
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- | Naive deep embedding: each operation is implemented as a
--   constructor.
type P s a = Parser1 s a
symbol :: P s s
symbol = Symbol

pfail :: P s a
pfail  = Fail

(+++) :: P s a -> P s a -> P s a
(+++)  = Choice

instance Monad (Parser1 s) where
  return  =  Return
  (>>=)   =  (:>>=)

data Parser1 s a where
  Symbol  ::  Parser1 s s
  Fail    ::  Parser1 s a
  Choice  ::  Parser1 s a -> Parser1 s a -> Parser1 s a
  Return  ::  a -> Parser1 s a
  (:>>=)  ::  Parser1 s a -> (a -> Parser1 s b) -> Parser1 s b

-- Final semantics to expose:
type Semantics s a = [s] -> [(a,[s])]

-- | Reference implementation/Semantics.  (It's easy to see that
--   it's what we want, but maybe inefficient.)
run :: Parser1 s a -> Semantics s a
run Symbol       = symbolS
run Fail         = failS
run (Choice p q) = choiceS (run p) (run q)
run (Return x)   = returnS x
run (p :>>= f)   = run p  `bindS`  (run . f)

symbolS :: Semantics s s -- [s] -> [(s, [s])]
symbolS = error "TBD"

failS   :: Semantics s a
failS   = error "TBD"

choiceS :: Semantics s a -> Semantics s a -> Semantics s a
choiceS = error "TBD"

returnS :: a -> Semantics s a 
returnS = error "TBD"

bindS   :: Semantics s a -> (a -> Semantics s b) -> Semantics s b
bindS   = error "TBD"

{- 

Using this reference semantics we can prove (exercise) a number
of useful laws about parsers. We will use these laws later to
derive an efficient implementation of the library.

  Notation: [| p |] = run p

  For two parsers p and q we define
  
    p == q  iff  ∀ s. [| p |] s == [| q |] s, 
      
      up to the order of elements in the result 
        (list is interpreted as a multiset).

  Monad Laws

    L1.  return x >>= f   ==  f x
    L2.  p >>= return     ==  p
    L3.  (p >>= f) >>= g  ==  p >>= (\x -> f x >>= g)

  More laws about >>=, (+++) and fail

    L4.  fail >>= f       ==  fail
    L5.  (p +++ q) >>= f  ==  (p >>= f) +++ (q >>= f)

  Laws about (+++) and fail

    L6.  fail +++ q       ==  q
    L7.  p +++ fail       ==  p

  Laws about (+++)

    L8.  (p +++ q) +++ r  ==  p +++ (q +++ r)
    L9.  p +++ q          ==  q +++ p           
         -- multisets are important in L9!

  Laws about >>=, (+++) and symbol
  
    L10. (symbol >>= f) +++ (symbol >>= g)
           ==  symbol >>= (\c -> f c +++ g c)

  Here is the proof of L10 for the case of a non-empty input
  string:

       [| (symbol >>= f) +++ (symbol >>= g) |] (c:s)         

  ==  { semantics of (+++) }

       [| symbol >>= f |] (c:s) ++ [| symbol >>= g |] (c:s)  

  ==  { semantics of >>= and symbol }

       [| f c |] s ++ [| g c |] s                   

  ==  { semantics of (+++) }

       [| f c +++ g c |] s                                  

  ==  { semantics of symbol and >>= }

      [| symbol >>= (\x -> f x +++ g x) |] (c:s)

  Exercise: prove or test the laws
-}

{- The reference semantics is useful for reasoning, but
   inefficient.  There are three sources of inefficiency that we
   can identify:

   1. The list comprehension builds a lot of intermediate lists
   which might be costly.

   2. List append (++) is linear in its first argument which
   means that left nested appl.s of (+++) get a quadratic
   behaviour.

   3. (+++) is treated in a depth first way, first computing the
   results of the left parser, then computing the results of the
   second parser. This leads to a space leak since we have to
   hang on to the full input string to feed to the second
   parser, while traversing the string with the first parser.
-}

-- To solve them we'll invent clever intermediate
--   representations.



-- Can we linearize sequencing (>>=)? (Would help with 1.)
data Parser2 s a where
    SymbolBind2  ::  (s -> Parser2 s a) -> Parser2 s a
    -- SymbolBind f  ≜  Symbol >>= f
    Return2      ::  a -> Parser2 s a
    Choice2      ::  Parser2 s a -> Parser2 s a -> Parser2 s a
    Fail2        ::  Parser2 s a


run2 :: Parser2 s a -> Semantics s a
run2 (SymbolBind2 f)  = symbolBind2S (\s -> run2 (f s))
run2 (Return2 y)      = returnS y
run2 (Choice2 p q)    = choiceS (run2 p) (run2 q)
run2 Fail2            = failS

symbolBind2S :: (s -> Semantics s a) -> Semantics s a
symbolBind2S = error "TBD"

-- Should satisfy:  symbolBind2S f == symbolS  `bindS`  f

{- 

  symbolS  `bindS`  f  

= { def. of bindS }
  
concatMap (uncurry f) . symbolS

= { def. of symbolS }

  \cs -> case cs of  []       -> concatMap (uncurry f) []
                     (c : s)  -> concatMap (uncurry f) [(c, s)]

= { concatMap lemmas  }

  \cs -> case cs of  []       -> []
                     (c : s)  -> uncurry f (c, s)

= { def. of uncurry }

  \cs -> case cs of  []       -> []
                     (c : s)  -> f c s
-}


-- It turns out that we can also translate Parser1 into Parser2.
p12 :: Parser1 s a -> Parser2 s a
p12 Symbol      =  SymbolBind2 Return2 -- L1
p12 Fail        =  Fail2
p12 (Choice p q)=  Choice2 (p12 p) (p12 q)
p12 (Return y)  =  Return2 y 
p12 (Symbol      :>>= k)  =  SymbolBind2 (p12 . k) 
                            -- def of SymbolBind
p12 (Fail        :>>= k)  =  Fail2 
                            -- Parser law. L4.
p12 ((Choice p q):>>= k)  =  Choice2 (p12 (p :>>= k))
                                     (p12 (q :>>= k))
                            -- Parser law. L5
p12 (Return y    :>>= k)  =  p12 (k y) 
                            -- monad law, L1
p12 ((p :>>= k') :>>= k)  =  p12 (p :>>= (\x -> k' x :>>= k)) 
                            -- monad law, L3

-- Can we linearize choice as well (+++)?
data Parser3 s a where
    SymbolBind3    ::  (s -> Parser3 s a) -> Parser3 s a
    ReturnChoice3  ::  a -> Parser3 s a   -> Parser3 s a 
    -- ReturnChoice x p  ≜  Return x +++ p
    Fail3          ::  Parser3 s a

run3 :: Parser3 s a -> Semantics s a
run3 (SymbolBind3 f)      []        =  []
run3 (SymbolBind3 f)      (s : ss)  =  run3 (f s) ss
run3 (ReturnChoice3 x p)  l         =  (x , l) : run3 p l 
                                -- ~= run (Return x +++ p)
run3 Fail3                l         =  []

-- But it turns out that we can translate 2 into 3!
p23 :: Parser2 s a -> Parser3 s a
p23 (SymbolBind2 f)  =  SymbolBind3 (p23 . f)
p23 (Return2 x)      =  ReturnChoice3 x Fail3 
                        -- def. of ReturnChoice
p23 (Choice2 p q)    =  best (p23 p) (p23 q)
p23 Fail2            =  Fail3

best :: Parser3 s a -> Parser3 s a -> Parser3 s a
best (SymbolBind3 f)      (SymbolBind3 g)     -- L10
  = SymbolBind3 (\s -> best (f s) (g s))   
best p                    (ReturnChoice3 x q) -- L8 (+++ commut)
  = ReturnChoice3 x (best p q)             
best (ReturnChoice3 x q)  p                   -- L9 (+++ assoc)
  = ReturnChoice3 x (best p q)
best p Fail3 = p   -- L6
best Fail3 q = q   -- L7


-- | Efficient implementation for general syntax:
parse :: P s a -> Semantics s a
parse = run3 . p23 . p12

-- we could show formally: 
-- (x , s) ∈ run  p ss  <=>  (x , s) ∈ run2 (p12 p) ss
-- (x , s) ∈ run2 p ss  <=>  (x , s) ∈ run3 (p23 p) ss

-- and therefore:
-- (x , s) ∈ run p ss   <=>  (x , s) ∈ parse p ss

-- Exercise: prove or test


{----------------------
 NOTES:

* L4 to L10 are "parser laws", expected to hold of any
  well-behaved parser.

-}

--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor (Parser1 s) where
  fmap = liftM
  
instance Applicative (Parser1 s) where
  pure   = return
  (<*>)  = ap
