{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Problem3.QuestionCode where
import Problem2.Prod -- contains the |Prod m n| monad instance from Problem 2
import qualified Control.Monad.Identity  as  CMI
import qualified Control.Monad.State     as  CMS
import qualified Control.Monad.Error     as  CME

instance (CMS.MonadState s m, CMS.MonadState s n) => CMS.MonadState s (Prod m n) where
  get    =  Prod (CMS.get, CMS.get)
  put s  =  Prod (CMS.put s, CMS.put s)
instance (CME.MonadError e m, CME.MonadError e n) => CME.MonadError e (Prod m n) where
  throwError e = Prod (CME.throwError e, CME.throwError e)
  catchError mnx f = Prod ( CME.catchError (fstP mnx) (fstP . f)
                          , CME.catchError (sndP mnx) (sndP . f) )

type Store  =  Integer
type Err    =  String
newtype Eval1 a = Eval1{ unEval1 :: CMS.StateT Store (CME.ErrorT Err CMI.Identity) a }
  deriving (Monad, CMS.MonadState Store, CME.MonadError Err)

newtype Eval2 a = Eval2{ unEval2 :: CME.ErrorT Err (CMS.StateT Store CMI.Identity) a }
  deriving (Monad, CMS.MonadState Store, CME.MonadError Err)

startStateFrom :: Monad m => state -> CMS.StateT state m a -> m a
startStateFrom = flip CMS.evalStateT

emptyStore :: Store
emptyStore = 0

runEval1 :: Eval1 a -> Either Err a
runEval1 = CMI.runIdentity . CME.runErrorT . startStateFrom  emptyStore . unEval1

runEval2 :: Eval2 a -> Either Err a
runEval2 = CMI.runIdentity . startStateFrom  emptyStore . CME.runErrorT . unEval2

(-*-) :: (a1->a2) -> (b1->b2) -> (a1,b1) -> (a2,b2)
f -*- g = \(a,b)->(f a, g b)

type Test = Prod Eval1 Eval2

check :: Test a -> (Either Err a, Either Err a)
check = (runEval1 -*- runEval2) . unProd

test1 :: (CME.MonadError Err m, CMS.MonadState Store m) => m Store
test1 = (do CMS.put 1738; CME.throwError "hello"; CMS.get)
          `CME.catchError` \e-> CMS.get

main = print (check test1)
