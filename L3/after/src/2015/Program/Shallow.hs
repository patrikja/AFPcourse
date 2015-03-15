{-|
  A simple embedded language for input/output. Shallow embedding.
-}
module Program.Shallow
  ( Input, Output
  , Program
  , putC, getC
  , run
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

type Input  = String
type Output = String

-- | Shallow embedding: programs are represented by their semantics
--   In this case a program is a function from the input to the 
--   result, the remaining input and the output.
type IOSem a = Input -> (a, Input, Output)
newtype Program a = P { unP :: IOSem a }

-- | Print a character.
putC :: Char -> Program ()
putC c = error "TBD"

-- | Read a character (if there is one).
getC :: Program (Maybe Char)
getC = P $ error "TBD"

-- Program is a monad, which provides us with a nice interface for
-- sequencing programs.
instance Monad Program where
  return  = returnP
  (>>=)   = bindP

returnP :: a -> Program a
returnP x = P $ error "TBD"

bindP :: Program a -> (a -> Program b) -> Program b
bindP p k = P $ error "TBD"

-- | Running a program is simply returning its semantics.
run :: Program a -> IOSem a
run = error "TBD"


--------
-- Preparing for the Functor-Applicative-Monad proposal:
--   https://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

-- | The following instances are valid for _all_ monads:
instance Functor Program where
  fmap = liftM
  
instance Applicative Program where
  pure   = return
  (<*>)  = ap

{-
-- Exercise: write direct implementations:
apP :: Program (a->b) -> Program a -> Program b
fmapP :: (a->b) -> Program a -> Program b
-}
