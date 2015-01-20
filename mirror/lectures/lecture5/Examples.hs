{-# LANGUAGE FlexibleContexts #-}
module Examples where
import qualified Control.Monad.Reader   as CMR (MonadReader)
import qualified Control.Monad.State    as CMS (MonadState)
import qualified Control.Monad.Error    as CME (MonadError)
import Interpreter4

run :: String -> Either Err Integer
run s = runEval1 $ eval $ parse s

examplePrograms :: [String]
examplePrograms =
  [ "1700+38"
  , "let x=1+2; x+x"
  , "let p=new 1; let q=new 1738; !(p+1)"
  , "!p+1738"
  , "(try !p catch 0)+1738"
  , "let one = new 1; \
    \let dummy = (try ((one := 2) + !7) catch 0); \
    \!one"
  ]
exampleRuns :: [Either Err Integer]
exampleRuns = map run examplePrograms

main :: IO ()
main = mapM_ print exampleRuns
-- For reference:
-- > mapM_ f x = sequence_ $ map f x

-- --------------

evalP :: ( CMR.MonadReader Env   m
         , CMS.MonadState  Store m
         , CME.MonadError  Err   m
         ) => String -> m Value
evalP = eval . parse


run' :: String -> (Either Err Value, Either Err Value)
run' s = ( runEval1 $ evalP s
         , runEval2 $ evalP s
         )
