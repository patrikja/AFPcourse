{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, FunctionalDependencies #-}
-- | Possible solution to problem 2.
module Types where

-- Problem a: associated types
class Stringly f where
  type Str f
  unstring :: Str f -> f
  string :: f -> Str f

instance Stringly Int where
  type Str Int = String
  unstring = read
  string = show

instance (Show a, Read a, Stringly b) => Stringly (a -> b) where
  type Str (a -> b) = String -> Str b
  unstring f = \x -> unstring (f $ show x)
  string f = \x -> string (f $ read x)




-- Problem b: multi parameter type classes
class Stringly2 f s where
  unstring2 :: s -> f
  string2 :: f -> s

instance Stringly2 Int String where
  unstring2 = read
  string2 = show

instance Stringly2 a b => Stringly2 (Int -> a) (String -> b) where
  unstring2 f = \x -> unstring2 (f $ show x)
  string2 f = \x -> string2 (f $ read x)
