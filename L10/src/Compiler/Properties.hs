module Main where
-- import Compiler.Parser
-- import Compiler.PropPretty
import Compiler.Behaviour   (Trace(..), cut, crashed)
import Compiler.Interpreter (interp)
import Compiler.Compiler    (compile)
import Compiler.Machine     (exec)
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..), Op1(..), Op2(..))
import Compiler.Pretty      ()

import Compiler.Generators  
  (arbExpr, shrinkExpr, shrinkCommand, 
   arbEnum, arbValue, shrinkValue)
import Compiler.TypedGenerators 
  (arbCommand, genEnv, genExpr, inferExpr, 
   typedShrink, typedShrinkE)

import Test.QuickCheck 

-- helper functions

(=~=) :: (Show a, Eq a) => Trace a -> Trace a -> Property
s =~= t = forAllShrink arbitrary shrink $ \(Positive n) ->
          let  s' = cut n s
               t' = cut n t
--          in s' == t'
          in whenFail (debug s t) (s' == t')
--          in  collect s' (s' == t')
  where
    debug s t = do
      putStrLn $ "lhs: " ++ show s
      putStrLn $ "rhs: " ++ show t

-- properties

prop_Congruence  = forAllShrink arbCommand shrink        $ \p ->
                     interp p =~= exec (compile p)

-- A "no shrinking" version:
prop_Congruence' = forAllShrink arbCommand shrinkNothing $ \p ->
                     interp p =~= exec (compile p)

--
canCrash p = case p of
    Skip       -> False
    Print _    -> False
    If c a b   -> canBeWrong c || any canCrash [a, b]
    While c a  -> canBeWrong c || canCrash a
    a :-> b    -> any canCrash [a, b]
    x := e     -> canBeWrong e
  where
    canBeWrong (Duo Div _ _)  = True
    canBeWrong (Duo Mod _ _)  = True
    canBeWrong (Duo _ a b)    = any canBeWrong [a, b]
    canBeWrong (Uno _ e)      = canBeWrong e
    canBeWrong (Val _)        = False
    canBeWrong (Var _)        = False


prop_crash =
  forAllShrink arbCommand typedShrink $ \p ->
  forAllShrink arbitrary shrink $ \(Positive n) ->
  let t = cut n (interp p) in
  whenFail (print t) $ crashed t ==> canCrash p

prop_typedExpr t =
  forAll genEnv $ \env ->
  forAllShrink (genExpr env t) typedShrinkE $ \e ->
    all ((t ==) . inferExpr env) $ typedShrinkE e

main = do
--  run 20 $ prop_typedExpr
--  run 200 $ prop_Congruence'   -- no shrinking
  run 200 $ prop_Congruence    -- shrinking
--  run  50 $ prop_crash
--  run 100 $ prop_pretty_parse
    
run n = quickCheckWith stdArgs{ maxSuccess = n }

----------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary  = arbExpr
  shrink     = shrinkExpr  

instance Arbitrary Command where
  arbitrary  = arbCommand
  shrink     = shrinkCommand             

instance Arbitrary Op1 where
  arbitrary = arbEnum
  -- no shrink

instance Arbitrary Op2 where
  arbitrary = arbEnum
  -- no shrink

instance Arbitrary Value where
  arbitrary = arbValue
  shrink    = shrinkValue


