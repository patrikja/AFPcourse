module DSL where
import Control.Monad
import Control.Applicative (pure, (<$>), (<*>))
import Data.Monoid
import Data.List(sortBy, sort)
import Test.QuickCheck -- not needed for the exam question

-- Exam question code:
empty    :: Doc
char     :: Char -> Doc
text     :: String -> Doc
line     :: Doc                -- newline
(<>)     :: Doc -> Doc -> Doc  -- append
union    :: Doc -> Doc -> Doc  -- a choice of two variants only differing in layout
prettys  :: Doc -> [String]

d1 = union  (text "x<-m") (text "x <- m")
d2 = union  (text "do {"  <> d1 <> char ';' <> text "f x}") 
            (text "do "   <> d1 <> line <>  text "   f x" )

-- Task (a): Implementation
data Doc = Empty | Char Char | Text String | Line 
         | Concat Doc Doc | Union Doc Doc 
  deriving (Show, Eq)
empty = Empty
char = Char
text = Text
line = Line
(<>) = Concat
union = Union

-- Note that lists form a Monad
prettys (Empty)       =  return ""
prettys (Char c)      =  return (c:[])
prettys (Text s)      =  return s
prettys (Line)        =  return "\n"
prettys (Concat x y)  =  sortBy cmp $ liftM2 (++) (prettys x) (prettys y)
prettys (Union  x y)  =  mergeOn width (prettys x) (prettys y)

cmp :: String -> String -> Ordering
cmp xs ys = compare (width xs) (width ys)

type Width = Int
width :: String -> Int
width "" = 0
width s = maximum $ map length $ lines s

mergeOn :: Ord a => (t -> a) -> [t] -> [t] -> [t]
mergeOn f [] ys = ys
mergeOn f xs [] = xs
mergeOn f (x:xs) (y:ys)  | f x <= f y  =  x : mergeOn f xs (y:ys)
                         | otherwise   =  y : mergeOn f (x:xs) ys
                                         
-- Task (b): Discussion
{-
Is your implementation deep or shallow? 

This implementation is deep. The constructors are trivial and the run
function |prettys| is doing all of the work.

Are you using any monads (explain)? 

The run function is using the list monad as a way of simulating a
non-deterministic choice of several alternatives. 

Would some of the API operations fit the |Monoid| type class
(explain)?

It seems natural to have (Doc, empty, (<>)) as a Monoid. We just have
to be careful with what equality means - for the above implementation
with "derived" Eq the laws don't hold. 

One way around it is to use "smart" constructors instead - the
smartapp below solves the first two monoid laws, but not the third.

Another way is to compare the semantics. Then we don't need any smart
constructors.

Note that (Doc, empty, union) is not a Monoid although union is
associative. This is because empty is an empty document, not an empty
list of documents. Also, union is not really mathematical union, but
cartesian product.

-}

---------
-- The rest is support code - not part of the expected exam answer.
instance Monoid Doc where
  mempty = empty
  mappend = smartapp
  
smartapp Empty m = m  
smartapp m Empty = m
smartapp m n = m <> n


law1' (==) z (+) m =   (z + m)  ==  m
law2' (==) z (+) m =   (m + z)  ==  m
law3' (==)   (+) m1 m2 m3 = ((m1 + m2) + m3)  ==  (m1 + (m2 + m3))

law1 e = law1' e mempty mappend
law2 e = law2' e mempty mappend
law3 :: Monoid t2 => (t2 -> t2 -> Bool) -> t2 -> t2 -> t2 -> Bool
law3 e = law3' e        mappend

synEq :: Doc -> Doc -> Bool
synEq = (==) -- derived, syntactic equality

semEq :: Doc -> Doc -> Bool
semEq x y = prettys x `bagEq` prettys y

bagEq x y = sort x == sort y      
  -- The third monoid law does not hold for list equality.
  -- This is because the sortBy only sorts down to length, not in more detail.

instance Arbitrary Doc where
  arbitrary = sized arbitraryDoc
  shrink = shrinkDoc  

arbitraryDoc :: Int -> Gen Doc  
arbitraryDoc n = oneof $ basecase ++ if n == 0 then [] else recurse
  where basecase = [pure empty, char <$> arbitrary, text <$> arbitrary, pure line]
        recurse  = [ (<>)  <$> arbitrary <*> arbitrary
                   , union <$> arbitrary <*> arbitrary
                   ]

shrinkDoc Empty        = []
shrinkDoc (Char 'x')   = [Empty]
shrinkDoc (Char c)     = [Empty, Char 'x']
shrinkDoc (Text s)     = Empty : map Char s -- ++ map Text (shrink s)
shrinkDoc Line         = [Empty]
shrinkDoc (Concat x y) = x : y : [Concat x' y | x' <- shrink x] 
                              ++ [Concat x  y'| y' <- shrink y]
shrinkDoc (Union  x y) = x : y : [Union x' y | x' <- shrink x] 
                              ++ [Union x  y'| y' <- shrink y]

main = do quickCheck (law1 semEq)
          quickCheck (law2 semEq)
          quickCheck (law3 semEq)
          quickCheck (law1 synEq)
          quickCheck (law2 synEq)
          quickCheck (expectFailure $ law3 synEq)
          quickCheck (expectFailure $ law1' semEq empty union)
          quickCheck (expectFailure $ law2' semEq empty union)
          quickCheck (law3' semEq union)

{- 

With list equality instead of bad equality, test3 is a simple test
case that fails.

*** Failed! Falsifiable (after 4 tests and 9 shrinks):  
-}

test3 = let d1 = Union Empty Empty     
            d2 = Union (Char 'x') Empty
            d3 = Union (Char 'y') Empty
            lhs = prettys $ (d1<>d2)<>d3
            rhs = prettys $ d1<>(d2<>d3)
        in (law3 semEq d1 d2 d3, lhs, rhs)

