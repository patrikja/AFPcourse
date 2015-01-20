{-|
  A simple embedded language for input/output. Shallow embedding.
-}
module Program.Shallow
  ( Input, Output
  , Program
  , putC, getC
  , run
  ) where

type Input  = String
type Output = String

-- | Shallow embedding: programs are represented by their semantics
--   In this case a program is a function from the input to the 
--   result, the remaining input and the output.
type IOSem a = Input -> (a, Input, Output)
newtype Program a = P { unP :: IOSem a }

-- | Print a character.
putC :: Char -> Program ()
putC c = P $ \i -> ((), i, [c])

-- | Read a character (if there is one).
getC :: Program (Maybe Char)
getC = P $ \i -> case i of
  []     ->  (Nothing,  [],  [])
  c : i' ->  (Just c,   i',  [])

-- Program is a monad, which provides us with a nice interface for
-- sequencing programs.
instance Monad Program where
  return  = returnP
  (>>=)   = bindP

returnP :: a -> Program a
returnP x = P $ \i -> (x, i, [])

bindP :: Program a -> (a -> Program b) -> Program b
bindP p k = P $ \i ->
    let  (x,  i1,  o1)  =  unP  p      i
         (y,  i2,  o2)  =  unP  (k x)  i1
    in   (y,  i2,  o1 ++ o2)

-- | Running a program is simply returning its semantics.
run :: Program a -> IOSem a
run = unP

