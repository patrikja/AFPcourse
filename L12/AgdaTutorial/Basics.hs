module Basics where
-- First some Haskell to show the connection
id1 ::  a -> a          
id1 = \ x -> x          
                        
id2 ::  a -> a          
id2 = \ x -> id1 (id1 x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose = \f g x -> f (g x)
