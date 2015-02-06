
-- | Testing the parser library from lecture 4.
module Main where

import Control.Applicative((<$>), (<*>))
import Control.Arrow(second)
import Data.List(sort)
import Test.QuickCheck
import Test.QuickCheck.Property (noShrinking)
-- import Parsers                   (P, symbol, (+++), pfail, parse)
import Compiler.PolyParser                   (P, symbol, (+++), pfail, parse)

-- We import the first naive implementation as the specification.
import qualified Parser1 as Spec (P, symbol, (+++), pfail, parse)

-- | We want to generate and show arbitrary parsers. This is tricky
-- for a polymorphic type, so to do this we restrict ourselves to
-- parsers of type P Bool Bool and build a datatype to model
-- these. This type basically captures recognizers of certain patterns
-- in raw binary data.
data ParsBB
  = Plus ParsBB ParsBB
  | Fail
  | Return Bool
  | Symbol
  | Bind ParsBB B2ParsBB -- the second type is needs to be filled in

-- | Instead of arbitrary functions (which quickCheck can generate but
-- not check equality of or print) we build a datatype modelling a few
-- interesting functions.
data B2ParsBB
  = K ParsBB          -- \_ -> p
  | If ParsBB ParsBB  -- \x -> if x then p1 else p2

-- Applying a function to an argument.
apply :: B2ParsBB -> Bool -> ParsBB
apply (K p)      _ = p
apply (If p1 p2) x = if x then p1 else p2

-- | We can show elements in our model, but not the parsers from the
--   implementation.
instance Show ParsBB where
  showsPrec n p = case p of
    Fail   -> showString "pfail"
    Symbol -> showString "symbol"
    Return x -> showParen (n > 2) $ showString "return " . shows x
    Plus p q -> showParen (n > 0) $ showsPrec 1 p
                                  . showString " +++ "
                                  . showsPrec 1 q
    Bind p f -> showParen (n > 1) $ showsPrec 2 p
                                  . showsBind f
    
-- | Just to make the output a bit more readable
showsBind :: B2ParsBB -> ShowS
showsBind (K p2) = showString " >> " . showsPrec 1 p2
showsBind f      = showString " >>= ". showsPrec 1 f
    
    
-- And we can show our functions. That would have been harder if
-- we had used real functions.
instance Show B2ParsBB where
  show (K p)      = "\\_ -> " ++ show p
  show (If p1 p2) = "\\x -> if x then " ++ show p1 ++
                               " else " ++ show p2

-- | Generating an arbitrary parser. Parameterised by a size argument
--   to ensure that we don't generate infinite parsers.
genParsBB :: Int -> Gen ParsBB
genParsBB 0 = oneof [ return Fail
                    , Return <$> arbitrary
                    , return Symbol ]
genParsBB n =
  frequency $
    [ (1, genParsBB 0)
    , (3, Plus <$> gen2 <*> gen2)
    , (5, Bind <$> gen2 <*> genFun2)
    ]
  where
    gen2    = genParsBB (n `div` 2)
    genFun2 = genFun (n `div` 2)

-- | Generating arbitrary functions in our model.
genFun :: Int -> Gen B2ParsBB
genFun n = oneof $
  [ K  <$> genParsBB n    -- ^ Half of our functions are constant
  , If <$> gen2 <*> gen2
  ]
  where
    gen2 = genParsBB (n `div` 2)

instance Arbitrary ParsBB where
  arbitrary = sized genParsBB

  -- Shrinking is used to get minimal counter examples and is very
  -- handy.  The shrink function returns a list of things that are
  -- smaller (in some way) than the argument.
  shrink (Plus p1 p2) = p1 : p2 :
    [ Plus p1' p2 | p1' <- shrink p1 ] ++
    [ Plus p1 p2' | p2' <- shrink p2 ]
  shrink Fail         = [ Return False ]
  shrink (Return x)   = []
  shrink Symbol       = [ Return False ]
  shrink (Bind p k)   = p : apply k False : apply k True :
    [ Bind p' k | p' <- shrink p ] ++
    [ Bind p k' | k' <- shrink k ]

instance Arbitrary B2ParsBB where
  arbitrary = sized genFun

  shrink (K p)      = [ K p | p <- shrink p ]
  shrink (If p1 p2) = K p1 : K p2 :
    [ If p1 p2 | p1 <- shrink p1 ] ++
    [ If p1 p2 | p2 <- shrink p2 ]

-- | We can turn a parser in our model into its specification...
spec :: ParsBB -> Spec.P Bool Bool
spec Symbol        = Spec.symbol
spec (Return x)    = return x
spec (Plus p1 p2)  = spec p1   Spec.+++   spec p2
spec Fail          = Spec.pfail
spec (Bind p k)    = spec p >>= \x -> spec (apply k x)

-- | ... or we can compile to a parser from the implementation we're
--   testing.
compile :: ParsBB -> P Bool Bool
compile Symbol        = symbol
compile (Return x)    = return x
compile (Plus p1 p2)  = compile p1 +++ compile p2
compile Fail          = pfail
compile (Bind p k)    = compile p >>= compileFun k

compileFun :: B2ParsBB -> (Bool -> P Bool Bool)
compileFun k = \x -> compile (apply k x)

-- Tests

infix 0 =~=

-- | When are two parsers equal? Remember that we don't care
--   about the order of results so we sort the result lists
--   before comparing.
-- (=~=) :: P Bool Bool -> P Bool Bool -> Property
p =~= q = \s -> parse p s  `bagEq`  parse q s

bagEq :: Ord a => [a] -> [a] -> Bool
bagEq xs ys = sort xs == sort ys

-- We can turn all the laws we had into properties.
-- Exercise: check all the laws L1 .. L10.

law1'' x f =   (return x >>= f)   ==   (f x)

law1' x f =   return x >>= f   =~=   f x

law1 x f0 =   return x >>= f   =~=   f x
  where f = compileFun f0

law2 p0 =     p >>= return   =~=   p
  where p = compile p0

law3 p0 f0 g0 =  (p >>= f) >>= g  =~=  p >>= (\x -> f x >>= g)
  where p = compile p0
        f = compileFun f0
        g = compileFun g0

law5 p0 q0 f0 =   (p +++ q) >>= f  =~=  (p >>= f) +++ (q >>= f)
  where p = compile p0
        q = compile q0
        f = compileFun f0

law9 p0 q0 =      p +++ q  =~=  q +++ p
  where p = compile p0
        q = compile q0

-- | We can also check that the implementation behaves as the
--   specification.
prop_spec p s  = -- whenFail debug $ 
                 lhs  `bagEq`  rhs
  where
    lhs = parse    (compile p) s
    rhs = Spec.parse (spec p)  s
    debug = do putStrLn ("parse    (compile p) s = " ++ show lhs)
               putStrLn ("Spec.parse (spec p)  s = " ++ show rhs)
               putStrLn ("  where p = " ++ show p)
               putStrLn ("        s = " ++ show s)

----------------
-- | A utility function for turning off shrinking. Only used to
-- illustrate the strength of shrinking.
quickCheckNo law = quickCheck (noShrinking law)

-- ----------------------------------------------------------------
main = do           
  putStrLn "** prop_spec:"
  quickCheckNo prop_spec
--  quickCheck prop_spec
  
rest = do 
  putStrLn "** Monad law 1:"
  quickCheckNo law1
--  quickCheck law1

  putStrLn "** Monad law 2:"
  quickCheckNo law2
--  quickCheck law2

  putStrLn "** Monad law 3 (associativity):"
  quickCheckNo law3
--  quickCheck law3

  putStrLn "** law5 (dist >>= over +++):"
  quickCheckNo law5
--  quickCheck law5

  putStrLn "** law9 (x+y=y+x):"
  quickCheckNo law9
--  quickCheck law9



















----------------
-- Some unfinished experiments below          

language' :: (Bounded s, Enum s) => P s a -> [[s]]
language' p = filter (parseOK p) allLists

language :: ParsBB -> [[Bool]]
language = language' . compile

parseOK :: P s a -> [s] -> Bool
parseOK p = not . null . parse p

allLists :: (Bounded a, Enum a) => [[a]]
allLists = [] : [ x : xs | x <- allA, xs <- allLists ]
  where allA = [minBound .. maxBound]

test = do ps <- sample' (arbitrary :: Gen ParsBB)
          return $ zip ps (map language ps) 
