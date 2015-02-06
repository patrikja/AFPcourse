module Compiler.Machine(Instruction(..), exec) where

import Data.Array(Array, array, (!))
import Compiler.Behaviour (Trace(..))
import Compiler.Value     (Value(..), Op1, uno, Op2, duo)

data Instruction
  = Push Value
  | Pop
  | Fetch Int
  | Store Int
  | Instr1 Op1
  | Instr2 Op2
  | Display
  | Jump Int
  | JumpUnless Int
  | Halt
 deriving (Eq, Show)
 
type PC = Int
type Stack = [Value]

exec :: [Instruction] -> Trace Value
exec instrs = run 1 []
  where
  size   = length instrs
  memory :: Array PC Instruction
  memory = array (1,size) ([1..] `zip` instrs)
  run :: PC -> Stack -> Trace Value
  run pc stack =
    if pc < 1 || size < pc then Crash
    else
      case (memory ! pc, stack) of
      (Push x       , stack)          -> run pc' (x : stack)
      (Pop          , _ : stack)      -> run pc' stack
      (Fetch n      , stack)     
        | length stack >= n           -> run pc' (stack !! n : stack)
      (Store n      , x : stack)
        | length stack >= n           -> run pc' (take (n-1) stack ++ 
                                         -- Bug example
                                         -- run pc' (take n stack ++
                                                  x : drop n stack)
      (Instr1 op1   , i : stack)      -> run pc' (uno op1 i : stack)
      (Instr2 op2   , i : j : stack)  -> run pc' (duo op2 j i : stack)
      (Display      , i : stack)      -> i :> run pc' stack
      (Jump n       , stack)          -> step n (run (pc' + n) stack)
      (JumpUnless n , Bol b : stack)
        | b                           -> run pc' stack
        | otherwise                   -> step n (run (pc' + n) stack)
      (Halt         , stack)          -> End
      _                               -> Crash
     where
       pc' = pc + 1

step :: Int -> Trace Value -> Trace Value    
step n t | n < 0     = Step t
         | otherwise = t
