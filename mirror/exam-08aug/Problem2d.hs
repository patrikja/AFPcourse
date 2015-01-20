
module Problem2d where

-- We just show how to solve the problem in the shallow
-- embedding. The same solution works for the deep embedding
-- by moving the code into the run function.

type Doc = Int -> [String]

ind :: Int -> [String] -> [String]
ind i = map (replicate i ' ' ++)

($$) :: Doc -> Doc -> Doc
(f $$ g) w = f w ++ g w

text :: String -> Doc
text s _ = [s]

empty :: Doc
empty _ = []

-- An interesting question is how to distribute the width
-- between the arguments to (<>). Here we choose to let
-- the first argument take as much space as it wants.
(<>) :: Doc -> Doc -> Doc
(f <> g) w = hcat (f w) g
  where
    hcat []  g = g w
    hcat [x] g = case g (w - i) of
      []     -> [x]
      y : ys -> (x ++ y) : ind i ys
      where
        i = length x
    hcat (x : xs) g = x : hcat xs g

cat :: Doc -> Doc -> Doc
cat f g w
  | width tryH > w = (f $$ g) w
  | otherwise      = tryH
  where
    tryH = (f <> g) w

    width [] = 0
    width xs = maximum $ map length xs

render :: Int -> Doc -> String
render w f = unlines (f w)

-- Derived combinators

indent :: Int -> Doc -> Doc
indent i f = text (replicate i ' ') <> f

(<+>) :: Doc -> Doc -> Doc
f <+> g = f <> indent 1 g

