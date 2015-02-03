module Problem2.NonLaws where
import Control.Exception as CE

-- Surjective pairing does not hold, examples are p1 and p2
p1, p2 :: (a, b)
p1 = undefined
p2 = (fst p1, snd p1)
-- They can be distinguished using seq (or pattern matching)

-- Eta-expansion does not hold, examples are f1 and f2
f1, f2 :: a->b
f1 = undefined
f2 = \x -> f1 x
-- They can be distinguished (only) using seq

----------------------------------------------------------------
-- The rest is not part of the exam question, but shows how to make the Haskell
--   compiler tell the difference

test1 x = CE.catch (x `seq` return False) $ return . constTrue
  where constTrue :: SomeException -> Bool
        constTrue _ = True

test2 x y = do x_bot <- test1 x
               y_bot <- test1 y
               print (x_bot == y_bot)


main = do putStr "Surjective pairing is "; test2 p1 p2
          putStr "Eta expansion is      "; test2 f1 f2

-- If the program would say True, that would not be a proof (it has to
-- hold for all values) but if it says False that is definite.
