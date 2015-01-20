module Types where

import qualified Prelude
import Prelude hiding ((>>=),return)
import Data.Monoid


class Monad1 m where
    return1  :: a -> m a
    bind1    :: m a -> (a -> m b) -> m b

class Functor m => Monad2 m where
    return :: a -> m a
    join   :: m (m a) -> m a

-- a)

bind2 :: Monad2 m => m a -> (a -> m b) -> m b
bind2 m f = join (fmap f m)

-- b)

join1 :: Monad1 m => m (m a) -> m a
join1 mm = bind1 mm id

-- c)

instance Monad2 Maybe where
    return = Just
    join = joinMaybe
           
joinMaybe :: Maybe (Maybe a) -> Maybe a           
joinMaybe (Just (Just x))  = Just x
joinMaybe _                = Nothing

-- d)

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap = fmapState 

fmapState :: (a -> b) -> State s a -> State s b
fmapState f (State m) = State $ \ s -> let  (a,    s') = m s 
                                       in   (f a,  s')

instance Monad2 (State s) where
    return  = returnState
    join    = joinState

returnState :: a -> State s a
returnState a  = State $ \ s -> (a, s)

joinState :: State s (State s a) -> State s a
joinState (State mm)   = State $ \ s ->
    let (m, s')  = mm s
    in  runState m s'

--  in  runState m s  -- planted bug
