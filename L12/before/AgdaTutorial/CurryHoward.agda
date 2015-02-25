-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
module CurryHoward where
⋆ = Set
{-
The Curry-Howard isomorphism provides a "Logic" reading of types:
  p : P               p is a proof of P
                      
  FALSE = empty type  False
  non-empty type      True
  P -> Q              P implies Q
  (a : A) -> P        ∀a:A. P
  ...

-}
-- Propositions as types in Agda

data FALSE : ⋆ where   -- empty type

data TRUE : ⋆ where    -- non-empty type, Haskell type: ()
  unit : TRUE          -- any constructor name would do 

data _∧_ (P Q : ⋆) : ⋆ where  -- the wedge is pronounced "and"
  _,_ : P  ->  Q  ->  P ∧ Q

-- A proof of (P ∧ Q) is a pair of
--   a proof of P   and
--   a proof of Q

-- TODO: Define another non-empty type, just as "true" as TRUE

-- ∨ is pronounced "or", 
data _∨_ (P Q : ⋆) : ⋆ where -- Either p q in Haskell
  inl : P -> P ∨ Q           --   Left  :: p -> Either p q
  inr : Q -> P ∨ Q           --   Right :: q -> Either p q

-- A proof of (P ∨ Q) is a either
--   a proof of P   or
--   a proof of Q

data ∃ (A : ⋆)(P : A -> ⋆) : ⋆  where
  ex : (wit : A) -> P wit -> ∃ A P

-- A proof of (∃ A P) is
--    a witness   and a proof of
--    P witness

-- Built-in:

-- ∀-quantification:
--  A proof of ((a : A) -> P a) is a function taking
--   a value a (of type A)   to
--   a proof of (P a)

-- Implication:
-- A proof of (P -> Q) is a function taking
--   a proof of P   to
--   a proof of Q

----------------
-- Negation

¬_ : ⋆ -> ⋆
¬ A  =  A -> FALSE

test1 : ¬ FALSE  -- a proof a "not false"
test1 = ?

test2 : ¬ ¬ TRUE    -- (TRUE → FALSE) → FALSE
test2 = ?


-- Some simple examples

const : {A B : ⋆} -> A -> (B -> A)
const pa pb = pa

swap : {P Q : ⋆} -> P ∧ Q -> Q ∧ P
swap (pa , pb) = (pb , pa)

{-
-- Bug / feature (limit / strength): the logic is _constructive_
excludedMiddle : (P : ⋆) -> (P  ∨  ¬ P)
excludedMiddle P = {!!} -- can not be implemented
-}

-- Next: Families.agda






-- Detail:
notNotEM : (P : ⋆) -> ¬ ¬ (P  ∨  ¬ P)
notNotEM = ?
