
module Problem2c where

import Data.List

data Doc = Doc :$$ Doc
         | Doc :<> Doc
         | Empty
         | Text String
  deriving Show

($$) :: Doc -> Doc -> Doc
($$) = (:$$)

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
(<>) = (:<>)

text :: String -> Doc
text = Text

-- Derived combinators
indent :: Int -> Doc -> Doc
indent n x = text (replicate n ' ') <> x

(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> indent 1 y

-- Run function
render :: Doc -> String
render = unlines . toLines

toLines :: Doc -> [String]
toLines (x :$$ y) = toLines x ++ toLines y
toLines Empty     = []
toLines (Text s)  = [s]
toLines (x :<> y) = toLines x `hcat` toLines y
  where
    -- Same as the shallow implementation of (<>)
    hcat []       ys       = ys
    hcat xs       []       = xs
    hcat [x]      (y : ys) = (x ++ y) : map (replicate (length x) ' ' ++) ys
    hcat (x : xs) ys       = x : hcat xs ys

