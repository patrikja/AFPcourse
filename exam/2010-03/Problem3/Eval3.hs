{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Problem3.Eval3 where
import qualified Control.Monad.Identity  as  CMI
import qualified Control.Monad.State     as  CMS
import qualified Control.Monad.Error     as  CME
import qualified Control.Monad.Reader    as  CMR
import Problem3.QuestionCode

{-
newtype Eval1 a = Eval1{ unEval1 :: CMS.StateT Store (CME.ErrorT Err CMI.Identity) a }
  deriving (Monad, CMS.MonadState Store, CME.MonadError Err)
runEval1 :: Eval1 a -> Either Err a
runEval1 = CMI.runIdentity . CME.runErrorT . startStateFrom  emptyStore . unEval1
-}

type Env = String -- just a dummy - it could be anything
newtype Eval3 a = Eval3{ unEval3 :: CMR.ReaderT Env
                                      (CMS.StateT Store
                                         (CME.ErrorT Err
                                            CMI.Identity)) a }
  deriving (Monad, CMS.MonadState Store, CME.MonadError Err,
            CMR.MonadReader Env)

runEval3 :: Eval3 a -> Either Err a
runEval3 = CMI.runIdentity
         . CME.runErrorT
         . startStateFrom  emptyStore
         . startEnvIn myEnv
         . unEval3

startEnvIn :: Monad m => env -> CMR.ReaderT env m a -> m a
startEnvIn = flip CMR.runReaderT

myEnv :: Env
myEnv = "Szia világ"

-- Types: (done using where to check them)
runEval3' :: Eval3 a -> Either Err a
runEval3' (mx0 :: Eval3 a) = mx5
  where mx5 :: Either Err a
        mx5 = CMI.runIdentity mx4
        mx4 :: CMI.Identity (Either Err a)
        mx4 = CME.runErrorT mx3
        mx3 :: CME.ErrorT Err CMI.Identity a
        mx3 = startStateFrom  emptyStore mx2
        mx2 :: CMS.StateT Store (CME.ErrorT Err CMI.Identity) a
        mx2 = startEnvIn myEnv mx1
        mx1 :: CMR.ReaderT Env (CMS.StateT Store (CME.ErrorT Err CMI.Identity)) a
        mx1 = unEval3 mx0