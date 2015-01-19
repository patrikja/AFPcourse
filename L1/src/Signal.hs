-- | A very simple library for manipulating continuous signals.

-- 2015: We only looked at the module header in L1
module Signal
  ( Time
  -- | the 'Signal' type is abstract
  , Signal  
  -- * Smart constructors
  , constS, timeS
  -- * Combinators
  , ($$), mapT
  -- * Derived operation
  , mapS
  -- * Run function
  , sample
  ) where
import Control.Monad (forM_)
import Control.Applicative(Applicative(pure,(<*>)),(<$>),liftA2)

-- * Smart constructors
constS :: a -> Signal a
timeS  ::      Signal Time
-- * Combinators
($$)   :: Signal (a -> b) -> Signal a -> Signal b
mapT   :: (Time -> Time)  -> Signal a -> Signal a
-- * Derived operation
mapS   :: (a -> b)        -> Signal a -> Signal b
-- * Run function
sample :: Signal a -> Time -> a  

type Time = Double
newtype Signal a = Sig {unSig :: Time -> a}

-- | The constant signal.
constS x = Sig (const x)

-- | The time signal
timeS = Sig id

-- | Function application lifted to signals.
fs $$ xs = Sig (\t -> unSig fs t  (unSig xs t))

-- | Mapping a function over a signal.
mapS f xs = constS f $$ xs

-- | Transforming the time.
mapT f xs = Sig (unSig xs . f)

-- | Sampling a signal at a given time point.
-- This is the /semantic function/ of our library.
sample = unSig

--------------------------------------------
-- Examples

-- | sinusoidal of given frequency
sinS :: Double -> Signal Double
sinS freq = mapT (freq*) $ mapS sin timeS

test1 :: IO ()
test1 = magic $ sinS 0.2

test2 :: IO ()
test2 = magic $ avS (sinS 0.2) (sinS 0.1)

-- | Problem: averaging two signals?
-- @
-- averageS :: Fractional a => 
--             Signal a -> Signal a -> Signal a
-- averageS = -- ... Fill in a definition here ...
-- @
-- Hint: use the average function, mapS and $$ combinators.
average :: Fractional a =>  a -> a -> a
average x y = (x + y) / 2.0

avS xs ys = mapS average xs $$ ys

scale :: Num a =>  Signal a -> Signal a
scale = mapS ((30*) . (1+))

-- | Discretize a signal
discretize :: Signal Double -> Signal Int
discretize = mapS round

-- | convert to "analog"
toBars :: Signal Int -> Signal String
toBars = mapS (`replicate` '#') 

displayLength = 100
-- | display the signal at a number of points
display :: Signal String -> IO ()
display ss = forM_ [0..displayLength] $ \x ->
   putStrLn (sample ss x)

-- | The display magic.
-- Note how we take advantage of function composition, 
-- types defined so far, etc.
magic :: Signal Double -> IO ()
magic = display . toBars . discretize . scale

-- | Signal is an applicative functor
instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure  = constS
  (<*>) = ($$)


----------------------------------------------------
-- | Answer to exercise
averageS :: Fractional a => 
            Signal a -> Signal a -> Signal a
averageS xs ys = mapS average xs $$ ys

-- | It can also be generalised to an arbitray Applicative functor
averageA :: (Fractional a, Applicative f) => 
             f a -> f a -> f a
averageA xs ys = average <$> xs <*> ys
-- | or slightly shorter
averageA' :: (Fractional a, Applicative f) => 
             f a -> f a -> f a
averageA' = liftA2 average

-- | Control.Applicative:
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html>
