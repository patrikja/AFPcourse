{-# LANGUAGE GADTs #-}
module DSL where

-- data C -- To be defined
-- Primitive operations
inv    :: C   -> C      -- inverter (``not''-gate)
ands   :: [C] -> C      -- ``and''-gate with zero or more inputs and one output 
delay  :: C   -> C      -- delay the output one step
-- Derived operations
ors    :: [C] -> C      -- ``or''-gate with zero or more inputs and one output
xor    :: C -> C -> C   -- binary ``xor''-gate
false, true, toggle :: C
-- Run functions
run    :: C -> [Bool]
-- show   :: C -> String -- Not asked for, but could be derived.

-- Here is a selection of the properties it should satisfy:

prop_inv    i c  =  run (inv c) !! i  /=  run c !! i

prop_delay0   c  =  not (run (delay c) !! 0)
prop_delay  i c  =  run (delay c) !! (i+1)  ==  run c !! i

prop_true   i    =  run true !! i

prop_toggle i    =  run toggle !! i  ==  (i `mod` 2 == 1)

-- DLS a) Implement the derived operations while keeping the type |C|
-- abstract.

ors = inv . ands . map inv

xor c1 c2 = ands [ors [c1, c2], inv (ands [c1, c2])]

true  = ands []
false = inv true

toggle = delay (inv toggle)
--toggle = delay (xor true toggle)

----------------------------------------------------------------
-- DSL b) Implement the type |C|, the primitive operations and |run|
-- using a deep embedding.

data C where
    Inv    :: C    -> C
    Ands   :: [C]  -> C
    Delay  :: C    -> C
  deriving (Show)

inv    = Inv
ands   = Ands
delay  = Delay

run (Delay c)    = False : run c
run (Ands cs)    = map and $ trans $ map run cs
run (Inv c)      = map not $ run c

type Vec  n a = [a]  -- just for documentation
type Stream a = [a]  -- to keep vectors and streams apart

trans :: Vec n (Stream a) -> Stream (Vec n a)
trans []           = repeat []
trans ((x:xs):ss)  = (x : map head ss) : trans (xs : map tail ss)

-- Slow version (also OK)
trans' []     = repeat []
trans' (s:ss) = zipWith (:) s (trans' ss)
