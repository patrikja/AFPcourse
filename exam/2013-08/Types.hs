-- First some code provided in the exam question (from RWH ch18)
module Types (listDirectory, countEntries) where
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

-- Then a solution to the first subproblem

-- a) 
    
test = execWriterT (countEntries "T") >>= print
-- Output: [("T",2),("T/D",1),("T/A",2)]
-- or      [("T",2),("T/A",2),("T/D",1)]


{- 

At the top level, evaluation of |print|, |>>=| and |execWriterT|
forces the evaluation of |countEntries "T"|. The first call to
|listDirectory| returns |["A", "D"]| so that |tell| emits the pair
|("T", 2)| and the for-loop runs twice. In both runs the subdirectory
exists so |countEntries| is called recurively with |"T/A"| and with
|"T/D"|. These times |tell| emits |("T/A", 2)| and |("T/D", 1)| but
since there are no further subdirectories no further recursive calls
happen.

-}

-- b) 

-- I assume that a "recursion depth" of < 0 means "do nothing", depth
-- 0 means work through this directory but no subdirectories, etc.

countEntriesMax :: Int -> FilePath -> WriterT [(FilePath, Int)] IO ()
countEntriesMax n path | n < 0 = return ()          -- change
                       | otherwise = do             -- change
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntriesMax (n-1) newName      -- change
