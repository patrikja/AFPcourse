module Problem1.Test where
import qualified Control.Monad.State as CMS(StateT, runStateT)
import qualified Control.Monad.Error as CME()
import Problem1.Types(Err, Value)
import Problem1 -- (C(..), BOp(..), UOp(..), NullOp(..), eval)

----------------------------------------------------------------
-- Testing code:
-- run' ::                  CalcM Value -> Either Err Value
run' :: (Num s, Monad m) => CMS.StateT s m v -> m v
run' mv = do 
  (result, mem) <- CMS.runStateT mv 0
  return result          

-- run ::                        Calc -> Either Err Value
run :: (Fractional v, Monad m) => C v -> m v
run = run' . eval

----------------------------------------------------------------
-- Some instances just for fun
instance Monad C where
  return = returnC
  (>>=)  = bindC
  fail   = failC

returnC :: a -> C a
returnC = Num

bindC :: C a -> (a -> C b) -> C b
bindC (CBin  op e1 e2) f  =  CBin  op (bindC e1 f) (bindC e2 f)
bindC (CUn   op e)     f  =  CUn   op (bindC e f)
bindC (CNull op)       f  =  CNull op
bindC (Num a)          f  =  f a

failC :: String -> C a
failC err = error ("failC: "++err) 
-- a Fail constructor is missing to completely implement the Haskell Monad class

instance Num a => Num (C a) where
  (+) = CBin Add
  (-) = CBin Sub
  (*) = CBin Mul
  fromInteger = Num . fromInteger
  abs    = error "C has no abs constructor"
  signum = error "C has no signum constructor"

instance Fractional a => Fractional (C a) where
  (/) = CBin Div
  fromRational = Num . fromRational

m :: C v -> C v
m  = CUn M
mr :: C v
mr = CNull MR

-- e1, e2, e3 :: Calc
e1 :: Num t => t
e1 = 1 + 2*3
e2 :: Num v => C v
e2 = 6*2 - m (1*3)
e3 :: Fractional v => C v
e3 = e2/mr 

-- test1, test2, test3 :: Either Err Value
test1, test2, test3 :: (Fractional v, Monad m) => m v
test1 = run e1
test2 = run e2
test3 = run e3


prop_sanity :: Fractional v => C v -> Bool
prop_sanity e = run (m e / mr) == (Right 1 :: Num v => Either Err v)

main :: IO ()
main = print ([test1, test2, test3, run (1/0)] :: [Either Err Value])


----------------------------------------------------------------
-- Left-overs

{-
data Calc = CBin BOp Calc Calc 
          | CUn UOp Calc 
          | CNull NullOp 
          | Num Integer 
  deriving (Eq, Show)
-}
-- data Button = BDig Char | BBin BOp | BUn UOp | BNull NullOp    deriving (Eq, Show)
