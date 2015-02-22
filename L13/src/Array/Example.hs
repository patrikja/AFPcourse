module Array.Example where
import Array
import Array.ShowInstances
-- Example arrays
example1 :: Array (Array Int)
example1 = fromList $ map fromList [ [1..10]
                                   , [17, 38]
                                   , [ ]
                                   , [1,7,3,8]
                                   ]
-- ArrNested (ArrInt [1,2,3,4,5,6,7,8,9,10,17,38,1,7,3,8]) 
--           (ArrPair (ArrIndex (ArrInt [0,10,12,12])
--                     ArrSize  (ArrInt [10,2,0,4])))


example2 :: Array (Array (Int, Int))
example2 = fromList $ map fromList [ [(1,2),(3,4),(5,6)]
                                   , [(7,8)]
                                   , [(9,10),(11,12)]
                                   ]
