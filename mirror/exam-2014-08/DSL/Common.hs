module DSL.Common where

data Dir  = Hori | Vert deriving (Eq, Show)
type ArtStrs = [String]
type ArtSize = (Int, Int)
type Sem = (ArtSize, ArtStrs)

frameSem :: Sem -> Sem
frameSem ((rows, cols), ss) = ((rows + 2, cols + 2), topbot : map f ss ++ [topbot])
  where topbot = '+' : replicate cols '-' ++ "+"
        f s = '|':s++"|"

textSem :: Dir -> String -> Sem
textSem Hori s = embed [s]
textSem Vert s = embed (map (:[]) s)

-- Invariant: all string in ss must have the same length (or perhaps the first is longest)
embed :: ArtStrs -> Sem
embed ss = ((rows, cols), ss)
  where rows = length ss
        cols | rows == 0 = 0
             | otherwise = length (head ss)

aboveSem  :: Sem -> Sem -> Sem
aboveSem ((arows, acols), a) ((brows, bcols), b) = ((arows+brows, maxcols), extend maxcols (a ++ b))
  where maxcols = max acols bcols

extend :: Int -> ArtStrs -> ArtStrs
extend n = map (extOne n)

extOne :: Int -> String -> String
extOne n xs = take n (xs ++ repeat ' ')

besideSem :: Sem -> Sem -> Sem
besideSem ((arows, acols), a) ((brows, bcols), b) = 
    ((maxrows, acols + bcols), zipWith (++) (fillIn (maxrows, acols) a) 
                                            (fillIn (maxrows, bcols) b))
  where maxrows = max arows brows

-- make sure all lines lengths match and column lenghts match
fillIn :: ArtSize -> ArtStrs -> ArtStrs
fillIn (rows, cols) ss = extend cols rowsDone
  where  rowsDone :: ArtStrs
         rowsDone = take rows (ss ++ repeat [])

spaceSem :: ArtSize -> Sem
spaceSem shape = (shape, fillIn shape [])

----------------------------------------------------------------
-- Not asked for in the exam:

lineSem   :: Int -> Dir -> Sem
lineSem n Hori = embed [replicate n '-']
lineSem n Vert = embed (replicate n "|")

emptySem  :: Sem
emptySem = ((0, 0), [])
