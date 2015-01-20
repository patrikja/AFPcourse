module DSL.Prop where
import DSL.Derived
import Test.QuickCheck
import Control.Applicative hiding (empty)

{-
  What properties (or laws) do your functions have? Mention at least
  three non-trivial such ways in which your functions interact.
-}

-- This should perhaps only be for Strings without newlines
prop_text :: Dir -> String -> String -> Bool
prop_text d xs ys = combine d (text d xs) (text d ys) =^= text d (xs++ys)

(=^=) :: Art -> Art -> Bool
x =^= y = run x == run y

assoc :: (a -> a -> Bool) -> (a->a->a) -> a -> a -> a -> Bool
assoc (==) (+) a b c = ((a + b) + c) == (a + (b + c))

prop_assoc_beside = assoc (=^=) beside
prop_assoc_above  = assoc (=^=) above

main = do quickCheck prop_text
          quickCheck prop_assoc_above
          quickCheck prop_assoc_beside
          s example


example :: Art
example = b1 + space + frame (b2 / space / b3)
  where
    b1 = frame (vertical (show 1738)) 
    b2 = frame (horizontal "hi!")
    b3 = frame (horizontal "Patrik")
    space = horizontal "   "
    (+) = beside
    (/) = above

----------------
-- Testing code below: not asked for in the exam:

s :: Art -> IO ()
s = putStr . unlines . render

instance Arbitrary Dir where 
  arbitrary = elements [Hori, Vert]


instance Arbitrary Art where
  arbitrary = sized arbArt

-- derived operations: only using the API
arbArt n | n <= 1     = oneof [ text <$> arbitrary <*> arbStr, return empty, space <$> arbSize]
         | otherwise  = oneof [ above <$> arbArt n' <*> arbArt n'
                              , beside<$> arbArt n' <*> arbArt n'
                              , frame <$> arbArt (n-1)
                              ]
  where n' = n `div` 2

arbStr = arbitrary 
-- vectorOf 3 (choose ('a','z')) -- for readability when debuging

arbSize = do a <- choose (0,15)
             b <- choose (0,20)
             return (a, b)


{-
  -- not in exam question
  Line   :: Int -> Dir -> Art
-}
