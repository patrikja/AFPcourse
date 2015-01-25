-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import Turtle

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT hiding (Program)

runGraphical :: Program -> IO ()
runGraphical p = do
    (_progName, _args) <- getArgsAndInitialize
    window <- createWindow "Turtle!"
    reshapeCallback $= Just setDisplay
    displayCallback $= display
    setDisplay =<< get windowSize
    mainLoop
  where
    display :: DisplayCallback
    display = do
        clear [ColorBuffer]
        setColor 1 1 1
        triangleFan [(0,0),(0,300),(300,300),(300,0)]
        setColor 1 0 0
        line  (100, 200) (200, 100)
        setColor 0 0.75 0
        line  (100, 100) (200, 200)
        setColor 0 0 0.5
        line  (150, 100) (150, 200)
        setColor 0 0 0
        line  (100, 150) (200, 150)
        flush

setColor :: Double -> Double -> Double -> IO ()
setColor r g b = GL.color $ GL.Color3 (rtf r) (rtf g) (rtf b)
  where rtf = realToFrac :: Double -> GLdouble

conv :: (Integral a,Num b) => a -> b
conv = fromIntegral . toInteger

setDisplay :: Size -> IO ()
setDisplay (Size w h) = do
    viewport $= (Position 0 0,Size w h)
    GL.loadIdentity
    GLU.ortho2D 0 (conv w) (conv h) 0

triangleFan :: [(Int,Int)] -> IO ()
triangleFan xys = renderPrimitive TriangleFan $ sequence_
    [ vertex $ Vertex3 (conv x) (conv y) (0 :: GLint)
    | (x,y) <- xys
    ]

line :: (Int,Int) -> (Int,Int) -> IO ()
line (x0,y0) (x1,y1) = renderPrimitive Lines $ do
    vertex $ Vertex3 (conv x0) (conv y0) (0 :: GLint)
    vertex $ Vertex3 (conv x1) (conv y1) (0 :: GLint)

