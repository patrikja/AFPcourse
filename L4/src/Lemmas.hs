module Lemmas where

concatMapNilLemma :: Eq b => (a -> [b]) -> Bool
concatMapNilLemma g =           concatMap g []  == []

concatMapSingletonLemma :: Eq b => (a -> [b]) -> a -> Bool
concatMapSingletonLemma g x =   concatMap g [x] == g x

