-- | Robot implementation library - use this to solve question 1e.
module RobotLib where

-- | Gives the distance, in metres, to the nearest object in the robot's
--   line of sight.
opticalSensor :: IO (Maybe Double)
opticalSensor = do
  putStr "Distance to nearest object in LOS (empty = no object in LOS): "
  ln <- getLine
  case reads ln of
    [(dist, "")] -> return $ Just dist
    _            -> return Nothing

-- | Wait the specified number of seconds before resuming execution.
wait :: Double -> IO ()
wait t = putStrLn $ "Slept for " ++ show t ++ " seconds"

-- | Instantly set the robot's engine to the given speed.
--   The speed is given in metres per second.
setSpeed :: Double -> IO ()
setSpeed x = putStrLn $ "Set speed to " ++ show x

-- | Turn left by the given number of degrees.
turn :: Double -> IO ()
turn x = putStrLn $ "Turned " ++ show x ++ " degrees left"

-- | Gives the total number of live robots left in the arena.
liveRobots :: IO Int
liveRobots = do
  putStr "Robots alive: "
  ln <- getLine
  case reads ln of
    [(n, "")] -> return n
    _         -> putStrLn "That's not an integer!" >> liveRobots

attackIO :: IO ()
attackIO = putStrLn "smack!"
