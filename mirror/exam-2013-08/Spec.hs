module Spec where
import Prelude hiding (return, (>>=), (>>))

-- Part of the exam question:
data P s a

symbol  ::  P s s
pfail   ::  P s a
(+++)   ::  P s a -> P s a -> P s a
return  ::  a -> P s a
(>>=)   ::  P s a -> (a -> P s b) -> P s b
type Bag = []
(\/) = (++)
type Sem s a = [s] -> Bag (a, [s])
sem :: P s a -> Sem s a
symbol   = error "TBD"
pfail    = error "TBD"
(+++)    = error "TBD"
return   = error "TBD"
(>>=)    = error "TBD"
sem      = error "TBD"

a >> b = a >>= const b

-- Start of answers:

-- a) the value is [('h', ")")]

type ProofSteps = [] -- Poor man's proofs (only type checked)
proofSteps :: ProofSteps (Bag (Char, String))
proofSteps = 
  [ sem (symbol >> symbol) "(h)"
  , -- def. of |>>|
    sem (symbol >>= const symbol) "(h)"
  , -- Lemma1 + beta reduction
    sem symbol "h)"
  , -- sem.sym.1
    [('h', ")")]
  ]
    
    
----------------------------------------------------------------
{-
Lemma1: sem (symbol >>= f) (s:ss)  =  sem (f s) ss

Lemma2: sem (symbol >>= f) []      =  []
-}


stepsEmpty :: (s -> P s a) -> (s -> P s a) -> ProofSteps [(a, [s])]
stepsEmpty f g = 
  [ sem ((symbol >>= f) +++ (symbol >>= g)) []
  , --  Spec. of sem (p +++ q)
    sem (symbol >>= f) []  \/  sem (symbol >>= g) []
  , --  Lemma2 twice
    []  \/  []
  , --  bag laws
    []
  , --  Lemma2 "backwards"
    sem (symbol >>= \s -> f s +++ g s) []
  ]

stepsCons :: (s -> P s a) -> (s -> P s a) -> s -> [s] -> ProofSteps [(a, [s])]
stepsCons f g s ss = 
  [ sem ((symbol >>= f) +++ (symbol >>= g)) (s:ss)         
  , --  sem.+++
    sem (symbol >>= f) (s:ss)  \/  sem (symbol >>= g) (s:ss)
  , --  Lemma1 twice
    sem (f s) ss  \/  sem (g s) ss                            
  , --  Spec. of sem (p +++ q) "backwards" }
    sem (f s +++ g s) ss                                   
  , --  Eta expansion
    sem ((\s -> f s +++ g s) s) ss                                   
  , --  Lemma 1 "backwards"
    sem (symbol >>= (\s -> f s +++ g s)) (s:ss)
  ]
