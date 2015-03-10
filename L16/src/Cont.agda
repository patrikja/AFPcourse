module Cont where

-- Agda is more helpful when it comes to filling in polymorphic code
Cont : Set -> Set -> Set
Cont r a = (a -> r) -> r

apC : {a b r : Set} -> Cont r (a -> b) -> Cont r a -> Cont r b
apC gf ga = \kb -> ga (\ka → gf (\kf → kb (kf ka)))
