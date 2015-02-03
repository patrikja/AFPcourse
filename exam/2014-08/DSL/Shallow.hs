module DSL.Shallow (module DSL.Shallow, module DSL.Common) where
import DSL.Common

{-
  Describe what a \emph{shallow} implementation could look like. Give
  a type definition and describe (in words or code) what each of your
  primitive operations, and your run function, would do.
-}

type Shallow = Sem
newtype Art = Art {run :: Shallow}
  deriving (Show, Eq)

frame :: Art -> Art
frame (Art a) = Art (frameSem a)

above, beside :: Art -> Art -> Art
above  (Art a) (Art b) = Art (aboveSem a b)
beside (Art a) (Art b) = Art (besideSem a b)

text :: Dir -> String -> Art
text d s = Art (textSem d s)

empty  :: Art
empty = Art ((0, 0), [])

space :: ArtSize -> Art
space s = Art (spaceSem s)




-- ----------------------------------------------------------------
-- Not in the exam question
line   :: Int -> Dir -> Art
line n Hori = Art $ embed [replicate n '-']
line n Vert = Art $ embed (replicate n "|")

