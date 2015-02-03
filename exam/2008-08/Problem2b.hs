
module Problem2b where

type Doc = [String]

($$) :: Doc -> Doc -> Doc
($$) = (++)

(<>) :: Doc -> Doc -> Doc
[]  <> ys       = ys
xs  <> []       = xs
[x] <> (y : ys) = (x ++ y) : map (replicate (length x) ' ' ++) ys
(x : xs) <> ys  = x : (xs <> ys)

text :: String -> Doc
text s = [s]

render :: Doc -> String
render = unlines

empty :: Doc
empty = []

-- Answer to problem 2a:

-- indent can be defined in terms of (<>) and text
indent :: Int -> Doc -> Doc
indent n xs = text (replicate n ' ') <> xs

-- (<+>) can be defined in terms of (<>) and indent.
(<+>) :: Doc -> Doc -> Doc
xs <+> ys = xs <> indent 1 ys

-- Alternatively (inlining the definition of indent)
-- xs <+> ys = xs <> text " " <> ys

-- Note that empty and text "" are different:
true1 = render (text "A" $$ empty   $$ text "B") == "A\nB\n"
true2 = render (text "A" $$ text "" $$ text "B") == "A\n\nB\n"

