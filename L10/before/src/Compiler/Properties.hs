module Compiler.Properties where
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..), Op1(..), Op2(..))
import Compiler.Interpreter (interp)
import Compiler.Compiler    (compile)
import Compiler.Machine     (exec, Instruction)
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

instance Arbitrary Op1 where
  arbitrary = arbEnum
  -- no shrink

instance Arbitrary Op2 where
  arbitrary = arbEnum
  -- no shrink

instance Arbitrary Value where
  arbitrary = arbValue
  shrink    = shrinkValue

arbCom1 :: Gen Command    
arbCom1 = return Skip  -- ** Not very arbitrary!! TBD.

arbCom2 :: Gen Command
arbCom2 = oneof [ return Skip 
                , liftM2 (:->) arbitrary arbitrary
                ]
-- Incomplete generator can be found through Haskell program coverage (hpc)

whileTrue :: Command -> Command
whileTrue c = While (Val (Bol True)) c

testProg1 :: Command
testProg1 = whileTrue Skip

testCompile :: [Instruction]
testCompile = compile testProg1

testExec :: Trace Value
testExec = exec testCompile

testInterp :: Trace Value
testInterp = interp testProg1

testProg2 :: Command
testProg2 = If (Val (Bol False)) Skip (Print (Val (Num 0)))

