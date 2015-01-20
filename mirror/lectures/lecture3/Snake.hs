{-
  A simple single player snake game.
-}
module Snake where

import ANSI    (ansiGoto, ansiColour, Colour(..))
import Program (Program, putS, getC)
import Game    (runGame, Game)
import Coord   (Coord, Dir(..), outOfBounds, movePos)

-- | A snake is a list of body coord.s and a dir. of travel.
data Snake = Snake { pos :: [Coord]
                   , dir :: Dir
                   }

-- | The starting position of the snake.
startingSnake :: Snake
startingSnake = Snake ((11,10) : replicate 20 (10,10)) East

-- | Check if a snake has collided with itself.
collision :: Snake -> Bool
collision g = case pos g of
  []      -> False
  p : ps  -> outOfBounds p  ||  p `elem` ps

-- | Output a string at a given coordinate (uses some ANSI magic).
putStrAt :: Coord -> String -> Program ()
putStrAt p s = putS $ gotoPos p ++ s
  where
    gotoPos (x, y) = ansiGoto (x * 2 + 1) (y + 1)

-- | Draw the snake. The last part of the tail is erased.
drawSnake :: Colour -> String -> Snake -> Program ()
drawSnake col px s = do
  let ps = pos s
  putStrAt (last ps) "  "                 -- erase previous tail
  putStrAt (head ps) $ ansiColour col px  -- print new head

-- | The different actions that the player can take.
data Action = Turn Dir | Exit deriving Show

-- | Keyboard controls. Binds keys to actions.
controls :: [(Char, Action)]
controls =
  zip "wasd" (map Turn [North, West, South, East]) ++
  [ ('q', Exit), ('\ESC', Exit) ]

-- | One step of the actual game
snake :: Game Snake
snake g 
  | collision g = do
      putStrAt (5, 7) "Game Over!"
      stop
  | otherwise = do
      drawSnake Yellow  "()" g
      putStrAt (0,0) ""
      mc <- getC
      case mc >>= \c -> lookup c controls of  -- Maybe is a monad
        Nothing       -> continue_
        Just (Turn d) -> continue d
        Just Exit     -> stop
  where
    -- Moving the snake means adding a new head and removing 
    -- the last element of the tail.
    move (p:ps) d = movePos p d : p : init ps

    stop          = return Nothing
    continue_     = continue (dir g)
    continue d    = return $ Just $ g { pos = move (pos g) d
                                      , dir = d }
