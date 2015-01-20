module DSL where
import Prelude hiding (sequence)
import Control.Monad.State as CMS hiding (sequence)
import System.Random (StdGen, randomR, newStdGen)

type Sem a = CMS.State StdGen a

-- |nextBoundedBy bound| for |0 < bound| returns a random result |0 <= result < bound|
nextBoundedBy :: Int -> Sem Int
nextBoundedBy bound = state $ randomR (0,bound-1)
  -- Not asked for in the exam question

-- a)

elements     = oneof . map returnGen

oneof        = frequency . map ((,) 1) -- Alternatively: zip (repeat 1)

fmapGen f g  = bindGen g $ \x -> return $ f x

sequence []      = returnGen []
sequence (g:gs)  = bindGen g $ \x -> bindGen (sequence gs) $ \xs -> return (x:xs)

-- b)

type Gen a = Sem a

returnGen a = CMS.state $ \s -> (a, s)

bindGen g f = CMS.state $ \s -> let (a, s') = runState g s in runState (f a) s'

frequency igs = do  let tot = sum $ map fst igs
                    j <- nextBoundedBy tot    
                    pick j igs
                   
pick :: Int -> [(Int, a)] -> a
pick j [] = error "pick: Out of bound"
pick j ((i, a):ias) | j < i      = a
                    | otherwise  = pick (j-i) ias
                         
run :: Gen a -> Int -> StdGen -> [a]      
run g n = take n . runInf g

runInf :: State s a -> s -> [a]
runInf g = evalState (sequence $ repeat g)

----
-- Testing code - not part of the exam question
runIO :: Gen a -> Int -> IO [a]
runIO g n = run g n `fmap` newStdGen

test = runIO (frequency 
  [ (3, return False),
    (1,return True)
  ]) 10
