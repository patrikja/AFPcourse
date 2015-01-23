module Main where
import Snake(startingSnake, snake)
import Game(runGame)
-- | The main function. Just kicks off the game.
main :: IO ()
main = runGame 0.1 snake startingSnake

