module Problem3.Eval_expanded where
{-
-- Short answer:
  Eval1 a  ~=  Store -> Either Err (a, Store)
  Eval2 a  ~=  Store -> (Either Err a, Store)
-}

{-
-- Details:

  CMS.StateT s m a  ~=  s -> m (a, s)
  CME.ErrorT e m a  ~=  m (Either e a)
  CMI.Identity a    ~=  a
-}

{-
newtype Eval1 a = Eval1{ unEval1 :: CMS.StateT Store (CME.ErrorT Err CMI.Identity) a }

   Eval1 a
~= Store -> (CME.ErrorT Err CMI.Identity) (a, Store)
~= Store -> CMI.Identity (Either Err (a, Store))
~= Store -> Either Err (a, Store)
-}

{-
newtype Eval2 a = Eval2{ unEval2 :: CME.ErrorT Err (CMS.StateT Store CMI.Identity) a }

   Eval2 a
~= CME.ErrorT Err (CMS.StateT Store CMI.Identity) a
~= (CMS.StateT Store CMI.Identity) (Either Err a)
~= Store -> CMI.Identity (Either Err a, Store)
~= Store -> (Either Err a, Store)
-}
