
module Matrix
  ( Matrix, Vec, Point, Angle
  , vecX, vecY, ptX, ptY
  , matrix, point, vec
  , cross, mul, inv, sub, divide, rot
  ) where

type Angle  = Double
data Vec    = V { vecX, vecY :: Double }
type Point  = Vec
data Matrix = M Vec Vec

-- | Matrix creation
matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = M (V a b) (V c d)

-- | Vector creation
vec :: Double -> Double -> Vec
vec = V

-- | Point creation.
point :: Double -> Double -> Point
point = vec

-- | Cross product
cross :: Vec -> Vec -> Double
cross (V a b) (V c d) = a * c + b * d

-- | Matrix multiplication
mul :: Matrix -> Vec -> Vec
mul (M c1 c2) v = V (cross c1 v) (cross c2 v)

-- | Matrix inversion
inv :: Matrix -> Matrix
inv (M (V a b) (V c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- | Subtraction
sub :: Point -> Vec -> Point
sub (V x y) (V dx dy) = V (x - dx) (y - dy)

-- | Division (unscale;-)
divide :: Point -> Vec -> Point
divide (V x y) (V sx sy) = V (x / sx) (y / sy)

rot :: Angle -> Point -> Point
rot a p = V newx newy
  where newx = cross (V (cos  a) (sin a)) p
        newy = cross (V (-sin a) (cos a)) p

-- inv (matrix (cos a) (-sin a) (sin a) (cos a))
--   == matrix (cos a) (sin a) (-sin a) (cos a)

-- Run functions

ptX, ptY :: Point -> Double
ptX = vecX
ptY = vecY

