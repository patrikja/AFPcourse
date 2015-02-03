module Problem3 where
import qualified Problem3.Eval_expanded
import qualified Problem3.Main
import qualified Problem3.GenericType
import qualified Problem3.Eval3

-- Code from exam question: Problem3/QuestionCode.hs

{-
-- Problem 3(a): see Problem3/Eval_expanded.hs
What would the types |Eval1|, |Eval2| look like without using anything from |Control.Monad.*| (expand out the types and simplify away the |newtype|s)?

-- Problem 3(b): see Problem3/Main.hs
What does |main| print? Motivate.

-- Problem 3(c): see Problem3/GenericType.hs
At what type is |test1| used in |main|? Why is it defined with a more general type?

-- Problem 3(d): see Problem3/Eval3.hs
Use monad transformers to extend the original |Eval1|, |runEval1| to |Eval3|, |runEval3| adding read-only access to an environment |Env|. Annotate the definition of |runEval3| with the types at the intermediate stages of the ``composition pipeline''. (For the ``pipeline'' |f . g . h| that would be the return types of |g| and |h|.)

-}
