{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE RankNTypes #-}
module RankNTypes where

data V = I Int | D Double

f :: (forall a. Num a => a -> a -> a) -> V -> V -> V
f op (I a) (I b) = I (op a b)
f op (D a) (D b) = D (op a b)
f _  _     _     = error "mismatch"


-- q :: [forall a. a] -> [forall b. b ]
