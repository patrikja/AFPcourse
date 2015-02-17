module Main where
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..))
import Compiler.Interpreter (interp)
import Compiler.Compiler    (compile)
import Compiler.Machine     (exec)
import Compiler.Behaviour   (Trace(..), cut, crashed)
import Compiler.Pretty      ()
import Compiler.Generators

import Control.Monad
import Test.QuickCheck 

propCongruence :: Command -> Property
propCongruence p = collect (take 10 $ show p) $  
  interp p =~= exec (compile p)
-- 

-- (=~=) :: (Show a, Eq a) => Trace a -> Trace a -> Integer -> Bool
(=~=) :: (Show a, Eq a) => Trace a -> Trace a -> Positive Integer -> Property
s =~= t = \(Positive n) -> 
          let  s' = cut n s
               t' = cut n t
          in collect t' $ s' == t'
-- collect (crashed s') $ 
-- 

main :: IO ()
main = do quickCheck propCongruence

----------------
instance Arbitrary Command where
  arbitrary = arbCommand
  shrink _ = []  -- ** No shrinking => difficult debugging. TBD.

arbCom1 :: Gen Command    
arbCom1 = return Skip  -- ** Not very arbitrary!! TBD.

arbCom2 :: Gen Command
arbCom2 = oneof [ return Skip 
                , liftM2 (:->) arbitrary arbitrary
                ]

whileTrue :: Command -> Command
whileTrue c = While (Val (Bol True)) c

