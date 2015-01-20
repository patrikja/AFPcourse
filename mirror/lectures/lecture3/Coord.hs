module Coord where

-- | A screen coordinate is just a pair of integers (x,y).
type Coord = (Int, Int)

-- | Directions.
data Dir = North | East | South | West
  deriving (Eq, Show, Enum)

-- | Compute the adjacent coordinate in a given direction.
movePos :: Coord -> Dir -> Coord
movePos (x, y) d = case d of
  North -> (x,      y - 1)
  East  -> (x + 1,  y)
  South -> (x,      y + 1)
  West  -> (x - 1,  y)

outOfBounds :: Coord -> Bool
outOfBounds (x,y) = x<0 || y<0
-- could also check some maximum x and y
