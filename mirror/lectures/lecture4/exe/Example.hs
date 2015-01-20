module Main where
import Test.QuickCheck{- (Arbitrary(arbitrary), Gen,
                         sized, elements, frequency, 
                         quickCheck, sample) -}
import Control.Monad(liftM, liftM2)
import ParseUtil -- (P, symbol, pfail, (+++), parse, ...)
import Lemmas

-- | A very simple expression type.
data Expr = Lit Int | Plus Expr Expr
  deriving (Eq)
--  deriving (Eq, Show) -- for debugging

-- | A parser for expressions. E ::= T | T '+' E  
--                                =  T ( empty | '+' E)
--                                =  T ('+' T)*
{-
-- | One of many possible variants (this associates (+) to the right
-- which is not quite correct if Expr should be a subset of Haskell)

exprP :: P Char Expr
exprP = do t <- termP
           maybeParsePlusExpr t
  where maybeParsePlusExpr :: Expr -> P Char Expr
        maybeParsePlusExpr t = 
          return t +++ 
          do this '+'
             e <- exprP
             return (t `Plus` e)
-}

-- | A parser for terms.       T ::= int | '(' E ')'
termP :: P Char Expr
termP = 
  (do d <- digitP 
      return (Lit d))
   +++ 
  do this '('
     e <- exprP
     this ')'
     return e

test1 :: [(Expr, String)]
test1 = parse exprP "1+2+"

q1 = [ (Lit 1,                               "+2+3")
     , (Plus (Lit 1) (Lit 2),                  "+3")
     , (Plus (Lit 1) (Plus (Lit 2) (Lit 3)),     "")
     ]

-- [ (Lit 1, "+2+")
-- , (Plus (Lit 1) (Lit 2),"+")
-- ]

























exprP :: P Char Expr
exprP = do e <- termP 
           maybeSomeMoreAfter e
  where maybeSomeMoreAfter e1 = return e1 +++ do           
          this '+'
          e2 <- termP
          maybeSomeMoreAfter (Plus e1 e2)

{-
exprP :: P Char Expr
exprP = chainLeft plusP termP
  where
    -- Parse the plus sign. Returns the 'Plus' function.
    plusP :: P Char (Expr -> Expr -> Expr)
    plusP = this '+' >> return Plus

termP :: P Char Expr
termP = liftM Lit digitP +++ do
  this '('
  e <- exprP
  this ')'
  return e

-}



-- | We test that showing and then parsing is the identity and 
--   that the parse is unambiguous.
prop_parse :: Expr -> Bool
prop_parse e = [e] == parseAll (show e)
  where
    -- Throw away incomplete parses
    parseAll :: String -> [Expr]
    parseAll s = [ x | (x, "") <- parse exprP s ]
-- Bad:   
--    parseAll s = [ x | (x, _) <- parse exprP s ]

runTests = quickCheck prop_parse
              

main = runTests

-- quickCheck (\(Blind f) s -> concatMapSingletonLemma f s)

---------------------------
-- * Testing infrastructure

instance Show Expr where
  showsPrec p (Lit n)      = shows n
  showsPrec p (Plus e1 e2) = showParen (p > 0) $
    shows e1 . showString "+" . showsPrec 1 e2
-- | For reference:
-- > shows = showsPrec 0


type Size = Int
-- | Generating arbitrary expressions.
instance Arbitrary Expr where
  arbitrary = sized arb
    where
      digit :: Gen Int
      digit = elements [0..9]
      arb :: Size -> Gen Expr
      arb 0 = liftM Lit digit
      arb n = frequency $
          (1, arb 0) :
        [ (4, liftM2 Plus arb2 arb2) | n > 0 ]
        where
          arb2 :: Gen Expr
          arb2 = arb (n `div` 2)

