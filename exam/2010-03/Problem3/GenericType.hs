module Problem3.GenericType where
import Problem2.Prod (Prod)
import Problem3.QuestionCode (Eval1, Eval2, Store, test1)

-- The call of check forces test to have type Test State
--   because it must unify with (Test a) and with (m State)

-- The more general type of test1
-- 1. makes test1 reusable in different contexts
-- 2. works as documentation of the features used (MonadError and MonadState)

----------------
-- Not part of the exam question - examples of reuse of test1
test1_Eval1 = test1 :: Eval1 Store
test1_Eval2 = test1 :: Eval2 Store
test1_Prod  = test1 :: Prod Eval1 Eval2 Store
