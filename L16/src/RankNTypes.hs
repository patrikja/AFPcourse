{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE RankNTypes #-}
module Q where

data V = I Int | D Double

f :: (forall a. Num a => a -> a -> a) -> V -> V -> V
f op (I a) (I b) = I (op a b)
f op (D a) (D b) = D (op a b)



-- q :: [forall a. a] -> [forall b. b ]
