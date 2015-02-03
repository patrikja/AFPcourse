module Problem3.Main where
import qualified Problem3.QuestionCode as P3Q
main = P3Q.main

-- (Right 0,Right 1738)

{-
Motivation:
main runs the same program in the two monads Eval1 and Eval2

From 3(a) (Problem3.Eval_expanded) we know that Eval1 gives either an
error or a new store. Thus, even if the failure is handled, the
modified State will be thrown away. The program will thus return the
emptyStore, which is 0. Eval2, on the other hand, always returns the
new store, even in the case of error. Thus, the state modification
(CMS.put 1738) done before failing propagates through catch and is
visible in the result, which is 1738.

-}
