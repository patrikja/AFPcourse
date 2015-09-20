{-# LANGUAGE TypeFamilies #-} -- For the associated type Str f
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- | Possible solution to problem 2.
module Types where

-- Problem a: associated types
class Stringly f where
  type Str f
  unstring :: Str f -> f
  string   :: f -> Str f

instance Stringly Int where
  type Str Int = String
  unstring = read
  string   = show

instance (Show a, Read a, Stringly b) => Stringly (a -> b) where
  type Str (a -> b) = String -> Str b
  unstring f = \x -> unstring (f $ show x)
  string   f = \x -> string   (f $ read x)




-- Problem b: multi parameter type classes
class Stringly2 f s where
  unstring2 :: s -> f
  string2   :: f -> s

instance Stringly2 Int String where
  unstring2 = read
  string2   = show

instance Stringly2 a b => Stringly2 (Int -> a) (String -> b) where
  unstring2 f = \x -> unstring2 (f $ show x)
  string2   f = \x -> string2   (f $ read x)




----------------------------------------------------------------
-- Test code from the exam paper:

add :: Int -> Int -> Int
add = (+)

stringly_add :: String -> String -> String
stringly_add = string add

unstringly_add :: Int -> Int -> Int
unstringly_add = unstring stringly_add

prop_inverse :: (Int -> Int) -> Int -> Bool
prop_inverse = \f x -> f x == unstring (string f) x

prop_preserves_semantics :: (Int -> Int) -> Int -> Bool
prop_preserves_semantics = \f x -> show (f x) == string f (show x)
