module Compiler.PropPretty where
import Compiler.Pretty
import Compiler.Parser
import Test.QuickCheck

-- Desired property: (does not hold)
prop_pretty_parse p = parse (show (pretty p)) == p

prop_pp p = whenFail debug $ parse (show (pretty p)) == p
  where debug = do putStrLn ("p = " ++ show p)
                   putStrLn ("pretty p = " ++ show (pretty p))
                   putStrLn ("parse (pretty p) = " ++ show (parse (show (pretty p))))
