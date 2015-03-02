{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}
module Promotion where
data Nat = Zero | Suc Nat
-- With the extension "DataKinds" data-definitions are "promoted":
-- So we get a new _kind_ Nat with (empty) datatypes
--   Zero :: Nat
--   Suc  :: Nat -> Nat

-- The Vec family is unchanged from Families.hs
-- Live: Copy from Families.hs and remove overlapping definitions



-- Mention "Promotion": https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html
