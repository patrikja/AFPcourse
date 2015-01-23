module RobberLanguage where
import Program
import Data.List((\\))
import Data.Char(toLower, isUpper)

-- Example:

vokaler      =  "aouåeiyäö"
konsonanter  =  ['a'..'z'] \\ vokaler
ärVokal     c = toLower c `elem` vokaler
ärKonsonant c = toLower c `elem` konsonanter

rövarspråket = do mc <- getC 
                  case mc of 
                    Nothing  ->  return ()
                    Just c   ->  putS (översättTecken c) >> rövarspråket

översättTecken :: Char -> String
översättTecken c  |  ärKonsonant c  =  c : o : [c]
                  |  otherwise      =          [c]
  where o | isUpper c = 'O'  
          | otherwise = 'o'







main = do putStrLn ("Result: "++ show res)
          putStrLn ("Remaining input: "++inp)
          putStrLn ("Output: "++out)
  where (res, inp, out) = run rövarspråket "Astrid Lindgren"
