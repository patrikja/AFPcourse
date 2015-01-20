{-
  A simple game engine for ascii games.
-}
module Game (Game, runGame) where

import System.Posix(usleep)
import System.IO(stdin, stdout, hSetBuffering, BufferMode(NoBuffering), hSetEcho)

import ANSI(ansiGoto, ansiClearScreen)
import Program(Program, runIONonBlocking)

-- | A game with state s is a program which given a game state computes the
--   next state, doing some input/output in the process.
-- 'Nothing' represents the end of the game.
type Game s = s -> Program (Maybe s)

-- | Run a game with a given delay between each states.
gameLoop :: Float -> Game s -> s -> IO ()
gameLoop dt step st = do
  r <- runIONonBlocking (step st)
  case r of
    Nothing   -> return ()
    Just st'  -> do
      usleep (round $ dt * 1000000)
      gameLoop dt step st'

-- | Top-level function for running a game.
runGame :: Float -> Game s -> s -> IO ()
runGame dt step st = do
  putStr ansiClearScreen
  hSetBuffering stdin  NoBuffering -- Don't wait for newline when reading from stdin
  hSetBuffering stdout NoBuffering -- Don't wait when writing either
  hSetEcho stdin False             -- Don't echo characters on stdin
  gameLoop dt step st
  hSetEcho stdin True              -- Turn echo back on.
  putStr $ ansiGoto 1 30
