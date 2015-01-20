{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
module Problem1.CalcMInstance where
import qualified Control.Monad.State as CMS (MonadState, get, put)
import Problem1.Types
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}
type CalcM = StateT Mem (Either Err)

instance Monad CalcM where return = returnM; (>>=)  = bindM; fail   = failM; 

returnM :: a -> CalcM a
returnM a = StateT (\s-> Right (a, s))

bindM :: CalcM a -> (a -> CalcM b) -> CalcM b
bindM mx f = StateT $ \s-> do -- for the (inner) error monad
  (a, s') <- runStateT mx s 
  runStateT (f a) s'

failM :: String -> CalcM a
failM err = StateT (\s-> fail err)

instance CMS.MonadState Mem (CalcM) where get = getM; put = putM;

getM :: CalcM Mem
getM = StateT $ \s-> return (s, s)

putM :: Mem -> CalcM ()
putM m = StateT $ \s -> return ((), m)
