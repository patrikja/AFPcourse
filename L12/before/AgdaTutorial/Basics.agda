-- (Tutorial by Ulf Norell - edited by Patrik Jansson.)
-- Each Agda file contains a top-level module, whose name
-- corresponds to the file name (just like in Haskell).

module Basics where
----------------------------------------------------------------
-- Expressions (types and terms)

-- The expression language of Agda is dependently typed
-- λ-calculus.
                                  -- Haskell (for comparison)
id₁ : (A : Set) -> A -> A         -- id1 ::  a -> a          
id₁ = ?                           -- id1 = \ x -> x          
-- Emacs: C-c C-r      refine
--        C-c <space>  give                                                             
id₂ : (A : Set) -> A -> A         -- id2 ::  a -> a          
id₂ = ?                           -- id2 = \ x -> id1 (id1 x)
--        C-c C-a      auto   

{- Haskell differences:
1. Single colon (:) for "has type" where Haskell uses double (::)
2. Type arguments are explicit by default
3. No upper- / lower-case rules for identifiers
4. Dependent function types available: f :  (a : A) -> B
5. Agda likes white space. This is not correct:
     id₁:(A:Set)->A->A
   Why not? In Agda the following strings are valid identifiers:
     "id₁:", "A:Set", "->A->A"

6. Set in Agda is the Haskell kind star (*). We'll use ⋆ for this
   for a while to avoid confusion with sets in mathematics.
-}

⋆ : Set1 -- An infinite tower of universes: Set : Set1 : Set2 ...
⋆ = Set

-- If you have problems reproducing unicode characters like ⋆
-- use M-x describe-char in emacs to find out what combination 
-- is used by the Agda input mode (here \* or \star)

-- Another useful function, featuring telescopes and typed λs.
compose :  (A B C : ⋆) -> (f : B -> C) -> (g : A -> B) -> (a : A) -> C
compose = ?

-- Exercise: write a dependently typed version of compose

----------------------------------------------------------------
-- Implicit arguments

-- Writing down type arguments explicitly soon gets boring.
-- Fortunately, Agda supports implicit arguments.

-- Note the {curlies} in the type and no A in the definition.
id₃ : {A : ⋆} -> A -> A
id₃ = ?

-- And it's not there when applying the function.
id₄ : {A : ⋆} -> A -> A
id₄ = ?

-- Now compose can be written infix
_∘_ :  {A B C : ⋆} -> (B -> C) -> (A -> B) -> (A -> C)
f ∘ g = ?
-- Emacs: C-c C-e    show context
--        C-c C-,    show goal and context

-- You can supply a hidden argument if wanted (or needed) by
-- enclosing it in {curly braces}. You can also hide any
-- parameter, not only type parameters.

-- Interesting though it is, eventually you'll get bored with the
-- pure λ-calculus...

-- Move on to: Datatypes.agda

















-- One solution
compose' : (A B : ⋆)(C : B -> ⋆)
           (f : (x : B) -> C x)(g : A -> B) ->
           (x : A) -> C (g x)
compose' = \A B C f g x -> f (g x)
