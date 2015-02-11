module Compiler.PropPretty where
import Compiler.Syntax (Command)
import Compiler.Pretty
import Compiler.Parser
import Test.QuickCheck

-- Desired property: (does not hold)
propPrettyParse :: Command -> Bool
propPrettyParse p = parse (show (pretty p)) == p

propPP :: Command -> Property
propPP p = whenFail debug $ parse (show (pretty p)) == p
  where debug = do putStrLn ("p = " ++ show p)
                   putStrLn ("pretty p = " ++ show (pretty p))
                   putStrLn ("parse (pretty p) = " ++ show (parse (show (pretty p))))
