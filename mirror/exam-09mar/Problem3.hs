
module Problem3 where

import Control.Monad.Reader

-- a)

newtype TC a = TC { runTC :: ReaderT Context (Either String) a }
  deriving (Monad, MonadError String, MonadReader Context)

-- b)

lookupVar :: Name -> TC Type
lookupVar x = do
  cxt <- ask
  case varType x cxt of
    Nothing -> typeError $ "not in scope: " ++ show x
    Just t  -> return t

extendContext :: Name -> Type -> TC a -> TC a
extendContext x t = local (addVar x t)

-- c)

infer (Var x) = lookupVar x
infer (Let x e1 e2) = do
  t1 <- infer e1
  extendContext x t1 (infer e2)

