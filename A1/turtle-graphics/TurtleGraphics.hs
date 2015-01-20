-- | A graphical run function for the turtle DSL
module TurtleGraphics (runGraphical) where

import Turtle

import Graphics.HGL

runGraphical :: Program -> IO ()
runGraphical p = runGraphics $ do
    w <- openWindowEx "Turtle!" Nothing (300, 300) DoubleBuffered (Just 1000)
    drawInWindow w (polygon [(0,0),(0,300),(300,300),(300,0)])
    onTick w [ withColor Red   $ line  (100, 200) (200, 100)
             , withColor Green $ line  (100, 100) (200, 200)
             , withColor Blue  $ line  (150, 100) (150, 200)
             , withColor Black $ line  (100, 150) (200, 150)
             ]
    getKey w >> return ()


onTick :: Window -> [Graphic] -> IO ()
onTick w []      = return ()
onTick w (x:xs)  = do
  getWindowTick w
  drawInWindow w x
  onTick w xs
