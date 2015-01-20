module DSL.Deep where
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

-- Deep embedded = far from the semantics, close to the syntax = AST datatype
data Shape = Empty | Disc   | Square | Translate Vec Shape | Scale Vec Shape | Rotate Angle Shape | Union Shape Shape | Intersect Shape Shape | Difference Shape Shape

empty       = Empty
disc        = Disc  
square      = Square
translate   = Translate
scale       = Scale
rotate      = Rotate
union       = Union
intersect   = Intersect
difference  = Difference

inside p Empty   =  False
inside p Disc    =  norm p <= 1
inside p Square  =  ordered [0, vecX p, 1] && ordered [0, vecY p, 1]
inside p (Translate   v  s)  =  inside (sub p v)     s
inside p (Scale       v  s)  =  inside (divide p v)  s
inside p (Rotate      a  s)  =  inside (rot (-a) p)  s
inside p (Union       x  y)  =  inside p x  ||  inside p y
inside p (Intersect   x  y)  =  inside p x  &&  inside p y
inside p (Difference  x  y)  =  inside p x  && not (inside p y)

-- Helper functions:

norm :: Vec -> Double
norm p = sqrt ((vecX p)^2 + (vecY p)^2)

ordered :: Ord a => [a] -> Bool
ordered (x:y:ys) = x <= y && ordered (y:ys)
ordered _        = True

----------------

