
module Problem1 where

-- a)

turn180  = left >>> left
right    = left >>> turn180
backward = turn180 >>> forward >>> turn180

-- b)

Forward s        >>> t = Forward (s >>> t)
TurnLeft s       >>> t = TurnLeft (s >>> t)
IfObstructed s t >>> u = IfObstructed (s >>> u) (t >>> u)
Idle             >>> s = s

-- c)

newtype S = S { unS :: Maze -> Ant -> Ant }

forward :: S
forward = S $ \maze ant ->
  if   obstructed maze ant
  then ant
  else moveAnt ant

ifObstructed :: S -> S -> S
ifObstructed (S s) (S t) = S $ \maze ant ->
  if   obstructed maze ant
  then s maze ant
  else t maze ant

idle :: S
idle = S $ \maze ant -> ant

left :: S
left = S $ \maze ant -> leftAnt ant

(>>>) :: S -> S -> S
S s >>> S t = S $ \maze ant -> t maze (s maze ant)

navigate :: Maze -> Ant -> S -> Ant
navigate maze ant (S s) = s maze ant

