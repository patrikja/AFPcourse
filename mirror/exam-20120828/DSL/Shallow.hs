module DSL.Shallow where
import Matrix  ( Vec, vecX, vecY  -- | :: Vec -> Double|
               , Angle            -- | = Double|
               , Point            -- | = Vec|
               , sub, divide      -- | :: Point  -> Vec    -> Point|
               , rot              -- | :: Angle  -> Point  -> Point|
               )   

empty   :: Shape
disc    :: Shape    -- disc with radius |1| around the origin
square  :: Shape    -- square between |(0,0)| and |(1,1)|    

translate   :: Vec    ->  Shape -> Shape  -- shift the shape along a vector                  
scale       :: Vec    ->  Shape -> Shape  -- magnify the shape by a vector                   
rotate      :: Angle  ->  Shape -> Shape  -- rotate the shape by an angle (around the origin)
union       :: Shape  ->  Shape -> Shape
intersect   :: Shape  ->  Shape -> Shape
difference  :: Shape  ->  Shape -> Shape

inside      :: Point ->  Shape -> Bool    -- run function: is the point inside the shape?

-- Shallow = close to the semantics, far from the syntax = almost the type of the run function
newtype Shape = Shape {runShape :: Point -> Bool}

inside = flip runShape

empty         = Shape (\p ->  False)
disc          = Shape (\p ->  norm p <= 1)
square        = Shape (\p ->  ordered [0, vecX p, 1] && ordered [0, vecY p, 1])
translate   v  s  =  Shape (\p -> inside (sub p v)     s)
scale       v  s  =  Shape (\p -> inside (divide p v)  s)
rotate      a  s  =  Shape (\p -> inside (rot (-a) p)  s)
union       x  y  =  Shape (\p -> inside p x  ||  inside p y)
intersect   x  y  =  Shape (\p -> inside p x  &&  inside p y)
difference  x  y  =  Shape (\p -> inside p x  && not (inside p y))

-- Helper functions:

norm :: Vec -> Double
norm p = sqrt ((vecX p)^2 + (vecY p)^2)

ordered :: Ord a => [a] -> Bool
ordered (x:y:ys) = x <= y && ordered (y:ys)
ordered _        = True

