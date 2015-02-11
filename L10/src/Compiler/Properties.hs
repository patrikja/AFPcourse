module Compiler.Properties where
-- import Compiler.Parser
-- import Compiler.PropPretty
import Compiler.Behaviour   (Trace(..), cut, crashed)
import Compiler.Interpreter (interp)
import Compiler.Compiler    (compile)
import Compiler.Machine     (exec)
import Compiler.Syntax      (Command(..), Expr(..))
import Compiler.Value       (Value(..), Op1(..), Op2(..))
import Compiler.Pretty      ()

import Compiler.Generators  
  (arbExpr, shrinkExpr, shrinkCommand, 
   arbEnum, arbValue, shrinkValue)
import Compiler.TypedGenerators 
  (arbCommand, genEnv, genExpr, inferExpr, 
   typedShrink, typedShrinkE, Type)

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
    debug lhs rhs = do
      putStrLn $ "lhs: " ++ show lhs
      putStrLn $ "rhs: " ++ show rhs

-- properties

propCongruence :: Property
propCongruence  = forAllShrink arbCommand shrink        $ \p ->
                    interp p =~= exec (compile p)

-- A "no shrinking" version:
propCongruence' :: Property
propCongruence' = forAllShrink arbCommand shrinkNothing $ \p ->
                    interp p =~= exec (compile p)

--
canCrash :: Command -> Bool
canCrash p = case p of
    Skip       -> False
    Print _    -> False
    If c a b   -> canBeWrong c || any canCrash [a, b]
    While c a  -> canBeWrong c || canCrash a
    a :-> b    -> any canCrash [a, b]
    _x := e    -> canBeWrong e
  where
    canBeWrong (Duo Div _ _)  = True
    canBeWrong (Duo Mod _ _)  = True
    canBeWrong (Duo _ a b)    = any canBeWrong [a, b]
    canBeWrong (Uno _ e)      = canBeWrong e
    canBeWrong (Val _)        = False
    canBeWrong (Var _)        = False


propCrash :: Property
propCrash =
  forAllShrink arbCommand typedShrink $ \p ->
  forAllShrink arbitrary shrink $ \(Positive n) ->
  let t = cut n (interp p) in
  whenFail (print t) $ crashed t ==> canCrash p

propTypedExpr :: Type -> Property
propTypedExpr t =
  forAll genEnv $ \env ->
  forAllShrink (genExpr env t) typedShrinkE $ \e ->
    all ((t ==) . inferExpr env) $ typedShrinkE e

main :: IO ()
main = do
--  run 20 $ propTypedExpr
--  run 200 $ propCongruence'   -- no shrinking
  run 200 $ propCongruence    -- shrinking
--  run  50 $ propCrash
--  run 100 $ propPrettyParse
    
run :: Testable prop => Int -> prop -> IO ()
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
