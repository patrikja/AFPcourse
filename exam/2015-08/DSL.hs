{-# LANGUAGE GADTs #-}
-- | One possible solution to problem 1.
module DSL where
import Data.Maybe (isJust)
import Control.Monad (liftM)
import RobotLib

-- Problem a: an API for the robot DSL and example implementation
-- Problem b: derived and primitive operations

-- Types and instances
type Degrees = Double
type Metres  = Double

instance Functor Robot where
  fmap f = (>>= return . f)

instance Applicative Robot where
  f <*> x = f >>= flip fmap x
  pure    = return

instance Monad Robot where
  return = Return
  (>>=)  = Bind

-- Primitive operations
left :: Degrees -> Robot ()
left = Turn

forward :: Metres -> Robot ()
forward = Move

attack :: Robot ()
attack = Attack

idle :: Robot ()
idle = return ()

if_ :: Robot Bool -> Robot a -> Robot a -> Robot a
if_ = If

enemiesLeft :: Robot Bool
enemiesLeft = EnemiesAlive

distToRobot :: Robot (Maybe Metres)
distToRobot = DistToRobot

-- Derived operations
right :: Degrees -> Robot ()
right = left . negate

backward :: Metres -> Robot ()
backward = forward . negate

while :: Robot Bool -> Robot () -> Robot ()
while cond act = if_ cond (act >> while cond act) (return ())

nextToRobot :: Robot Bool
nextToRobot = fmap (maybe False (<= 0.1)) distToRobot

facingRobot :: Robot Bool
facingRobot = fmap isJust distToRobot

-- Example strategy
strat :: Robot ()
strat = while enemiesLeft $ do
  if_ facingRobot attackSequence (left 2)
  where
    attackSequence = if_ nextToRobot (attack >> backward 0.5) (forward 0.1)




-- Problem c: laws
{-
    * left n >> left n == left (2*n)
    * left 360 >> p == p
    * forward n >> backward n == return () -- assuming no robot obstacle
    * if_ (pure True) p q == p
    * if_ (pure False) p q == q
    * p >> idle == idle >> p == p
    * Monad, applicative and functor laws are also fine here.
-}
-- Not required on the exam: Haskell code for testing the laws
-- Included here just for type checking the laws.

(===) :: Robot a -> Robot a -> Bool
(===) = error "Robot equality not implemented"

propLeftTwice :: Degrees -> Bool
propLeftTwice n = (left n >> left n) === left (2*n)

propLeft360 :: Robot a -> Bool
propLeft360 p = (left 360 >> p) === p

propIfTrue :: Robot a -> Robot a -> Bool
propIfTrue  p q = if_ (pure True)  p q === p

propIfFalse :: Robot a -> Robot a -> Bool
propIfFalse p q = if_ (pure False) p q === q

propIdle1 :: Robot () -> Bool
propIdle1 p = (p >> idle) === p

propIdle2 :: Robot a -> Bool
propIdle2 p = (idle >> p) === p

-- ...

-- Problem d: a deep embedding
data Robot a where
  Move         :: Metres -> Robot ()
  Turn         :: Degrees -> Robot ()
  Attack       :: Robot ()
  DistToRobot  :: Robot (Maybe Metres)
  If           :: Robot Bool -> Robot a -> Robot a -> Robot a
  Return       :: a -> Robot a
  Bind         :: Robot a -> (a -> Robot b) -> Robot b
  EnemiesAlive :: Robot Bool




-- Problem e: run function
runRobot :: Robot a -> IO a
runRobot Attack       = attackIO
runRobot (Turn d)     = turn d
runRobot (Move d)     = setSpeed d >> wait 1 >> setSpeed 0
runRobot DistToRobot  = opticalSensor
runRobot (If c a b)   = do
  c' <- runRobot c
  runRobot $ if c' then a else b
runRobot (Return x)   = return x
runRobot (Bind r f)   = do
  r' <- runRobot r
  runRobot (f r')
runRobot EnemiesAlive = liftM (>1) liveRobots
