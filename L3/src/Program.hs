-- | Using the 'Program' monad
module Program
  ( module ProgramImpl
  , putS, putSLn, getLn
  , runPut, run_, runIO, runIONonBlocking
  )
  where

import System.IO
import Program.Deep2 as ProgramImpl
-- import Program.Shallow as ProgramImpl

-- | Print a string.
putS :: String -> Program ()
putS = mapM_ putC

-- | Print a string and add a new line to the end.
putSLn :: String -> Program ()
putSLn s = putS (s ++ "\n")

-- | Read a line from the input.
getLn :: Program String
getLn = do
  mc <- getC
  case mc of
    Nothing   -> return ""
    Just '\n' -> return ""
    Just c    -> do
      s <- getLn
      return $ c : s

-- | Run function which throws away the remaining inputs.
run_ :: Program a -> Input -> (a, Output)
run_ p i = case run p i of
  (x, _, o) -> (x, o)

-- | Run a program p with input i as an IO computation writing to
--   stdout.
runPut :: Program b -> Input -> IO b
runPut p i = do 
  let (x, o) = run_ p i
  putStr o
  return x

-- | Run a program as an IO computation reading from stdin and
--   writing to stdout.
runIO :: Program a -> IO a
runIO p = getContents >>= runPut p

-- | Run a program on whatever is available on stdin at the moment.
--   Useful for writing event driven programs where you don't want
--   to block the program waiting for the user to press a key.
runIONonBlocking :: Program a -> IO a
runIONonBlocking p = getString >>= runPut p
  where
    -- Read as much from stdin as possible without blocking.
    getString = whileM (hReady stdin) getChar

-- | I wonder why this one isn't in the libraries...
whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond body = do
  ok <- cond
  if ok then do
      x <- body
      xs <- whileM cond body
      return (x : xs)
    else
      return []

