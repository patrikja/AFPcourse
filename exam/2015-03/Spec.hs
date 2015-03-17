{-# LANGUAGE ScopedTypeVariables #-}
module Spec where
import Prelude hiding (foldr, map)
{-
The \emph{list-fusion} law is as follows:

    f . foldr g b = foldr h c
  <== (is implied by)
    f b = c                 and
    f (g x y) = h x (f y)   for all x and y

  Give the polymorphic types for |f|, |g|, |b|, |h| and |c| and prove
  list-fusion for all finite lists by structural induction.

-}

-- Types: written in this way just to get the compiler to check them.
listFusionLaw :: (b -> c) ->       -- f
                 (a -> b -> b) ->  -- g
                 b ->              -- b
                 (a -> c -> c) ->  -- h
                 c ->              -- c
                 ([[a] -> c], [c], a -> b -> [c])
listFusionLaw f g b h c = ( [ f . foldr g b, foldr h c ]
                          , [ f b, c ]
                          , \x y -> [f (g x y) , h x (f y)]
                          )
-- For reference: the definition of foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _g b []     = b
foldr  g b (x:xs) = g x (foldr g b xs)

-- Problem 2a:

{- Proof by structural induction over lists of the property p:
p xs  =  (f . foldr g b) xs == (foldr h c) xs
      =  f (foldr g b xs) == foldr h c xs
Base case: xs = []
  f (foldr g b [])
== {Def. foldr}
  f b
== {Assumption}
  c
== {Def. foldr}
  foldr h c []

Step case: forall x, xs. p xs => p (x:xs)
let ih = p xs
in
  f (foldr g b (x:xs))
== {Def. foldr}
  f (g x (foldr g b xs))
== {Assumption}
  h x (f (foldr g b xs))
== {ih}
  h x (foldr h c xs)
== {Def. foldr}
  foldr h c (x:xs)

Thus by structural induction p xs holds for all (finite) xs.

-}

----------------
-- Problem 2b:
{-
  For which |g| and |a| is |map p = foldr g b|?  Use this form and
  list-fusion to prove that |foldr q r . map p| can be computed as a
  single |foldr|. Start by giving the polymorphic types of all the
  one-letter varaibles.
-}

map :: (a->b) -> [a] -> [b]
map (p::a->b) = foldr step base
  where base :: [b]
        base = []
        step :: a -> [b] -> [b]
        step = (:) . p
-- or simply
map' :: (a->b) -> [a] -> [b]
map' p = foldr ((:).p) []
        
-- Types:
checkTypes ::
  (b -> c -> c) ->  -- q
  c ->              -- r
  (a -> b) ->       -- p
  (a -> c -> c) ->  -- h
  c ->              -- c
  [ [a] -> c ]
checkTypes q r p h c = [foldr q r . map p, foldr h c]

{-
  foldr q r . map p = foldr h c
=  {Def. map}
  foldr q r . foldr ((:).p) [] = foldr s b  
<= {list-fusion with (f, g, b, h, c) = (foldr q r, ((:).p), [], h, c) }
    foldr q r [] = c                                and
    foldr q r (((:).p) x y) = h x (foldr q r y)     for all x and y
=  {Def. foldr and expanding ((:).p) x y = p x : y }
    r = c                                       and
    foldr q r (p x : y) = h x (foldr q r y)     for all x and y
=  {Def. foldr}
    r = c                                       and
    q (p x) (foldr q r y) = h x (foldr q r y)   for all x and y
<= {Generalise}
    r = c
    q (p x) z = h x z                           for all x and z
=  {Simplify}
    r = c
    h = q . p

To sum up:
  foldr q r . map p = foldr (q . p) r
-}


----------------------------------------------------------------

-- The rest is not part of the exam question - just included for type
-- checking the "proofs".
-- Exercise1: use QuickCheck to test the properties
-- Exercise2: write the proofs in Agda

listFusionProperty :: (b -> c) -> (a -> b -> b) -> b -> (a -> c -> c) -> c -> [a] -> [c]
listFusionProperty f g b h c xs  =
  [(f . foldr g b) xs, (foldr h c) xs, f (foldr g b xs), foldr h c xs]

listFusionBaseCase::
                 (b -> c) ->       -- f
                 (a -> b -> b) ->  -- g
                 b ->              -- b
                 (a -> c -> c) ->  -- h
                 c ->              -- c
                 [c]
listFusionBaseCase f g b h c = 
  [
    f (foldr g b [])
  , -- == {Def. foldr}
    f b
  , -- == {Assumption}
    c
  , -- == {Def. foldr}
    foldr h c []
  ]
  
listFusionStepCase ::
                 (b -> c) ->       -- f
                 (a -> b -> b) ->  -- g
                 b ->              -- b
                 (a -> c -> c) ->  -- h
                 c ->              -- c
                 a -> [a] ->
                 [c]
listFusionStepCase f g b h c x xs = 
     [
       f (foldr g b (x:xs))
     , -- == {Def. foldr}
       f (g x (foldr g b xs))
     , -- == {Assumption}
       h x (f (foldr g b xs))
     , -- == {ih}
       h x (foldr h c xs)
     , -- == {Def. foldr}
       foldr h c (x:xs)
     ]
----------------
-- Problem 2b:

foldMapFusion ::
  (b -> c -> c) ->  -- q
  c ->              -- r
  (a -> b) ->       -- p
  [ [a] -> c ]
foldMapFusion q r p = [foldr q r . map p, foldr (q . p) r]
