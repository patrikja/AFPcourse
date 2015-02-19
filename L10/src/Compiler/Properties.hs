module Compiler.Properties where
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..), Op1(..), Op2(..))
import Compiler.Interpreter (interp)
import Compiler.Compiler    (compile)
import Compiler.Machine     (exec, Instruction)
import Compiler.Behaviour   (Trace(..), cut, crashed)
import Compiler.Pretty      ()
import qualified Compiler.Generators       as G
import qualified Compiler.TypedGenerators  as TG

import Control.Monad
import Test.QuickCheck

-- helper functions

(=~=) :: (Show a, Eq a) => Trace a -> Trace a -> Property
s =~= t = forAllShrink arbitrary shrink $ \(Positive n) ->
          let  s' = cut n s
               t' = cut n t
          in whenFail (debug s t) (s' == t')
  where
    debug lhs rhs = do
      putStrLn $ "lhs: " ++ show lhs
      putStrLn $ "rhs: " ++ show rhs

-- properties

propCongruence :: Property
propCongruence  = forAllShrink G.arbCommand shrink        $ \p ->
                    interp p =~= exec (compile p)

-- A "no shrinking" version:
propCongruence' :: Property
propCongruence' = forAllShrink G.arbCommand shrinkNothing $ \p ->
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
  forAllShrink G.arbCommand TG.typedShrink $ \p ->
  forAllShrink arbitrary shrink $ \(Positive n) ->
  let t = cut n (interp p) in
  whenFail (print t) $ crashed t ==> canCrash p

propTypedExpr :: TG.Type -> Property
propTypedExpr t =
  forAll TG.genEnv $ \env ->
  forAllShrink (TG.genExpr env t) TG.typedShrinkE $ \e ->
    all ((t ==) . TG.inferExpr env) $ TG.typedShrinkE e

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
  arbitrary  = G.arbExpr
  shrink     = G.shrinkExpr

----------------
instance Arbitrary Command where
  arbitrary  = G.arbCommand
  shrink     = G.shrinkCommand

instance Arbitrary Op1 where
  arbitrary = G.arbEnum
  -- no shrink

instance Arbitrary Op2 where
  arbitrary = G.arbEnum
  -- no shrink

instance Arbitrary Value where
  arbitrary = G.arbValue
  shrink    = G.shrinkValue

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
