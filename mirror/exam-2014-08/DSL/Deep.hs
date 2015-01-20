{-# LANGUAGE GADTs #-}
module DSL.Deep (module DSL.Deep, module DSL.Common) where
import DSL.Common
{-
  Describe what a \emph{deep} implementation could look like. Give a
  type definition and describe (in words or code) what each of your primitive
  operations, and your run function, would do.
-}


data Deep where
  Text   :: Dir -> String -> Art
  Above  :: Art -> Art -> Art
  Beside :: Art -> Art -> Art
  Frame  :: Art -> Art
  -- not in exam question
  Line   :: Int -> Dir -> Art
  Empty  :: Art
  Space  :: ArtSize -> Art
  deriving (Eq, Show)
  
text = Text; frame = Frame; above = Above; beside  = Beside
line = Line; empty = Empty; space = Space

type Art = Deep

-- Run function:
run :: Art -> Sem
run (Text   s d) = textSem s d
run (Above  a b) = aboveSem   (run a) (run b)
run (Beside a b) = besideSem  (run a) (run b)
run (Frame a)    = frameSem   (run a)
run (Line n d)   = lineSem n d
run  Empty       = emptySem
run (Space size) = spaceSem size
