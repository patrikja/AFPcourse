module NotAFunctor where
-- Not a functor
type NotAFunctor = Equal
newtype Equal a = Equal {runEqual :: a -> a -> Bool}

fmapEqual :: (a -> b) -> Equal a -> Equal b
fmapEqual f (Equal op) = Equal $ \b1 b2 -> error "Hopeless!"

cofmapEqual :: (a -> b) -> Equal b -> Equal a
cofmapEqual f (Equal op) = Equal (\a1 a2 -> op (f a1) (f a2))

{-
fmap   :: Functor f   => (a -> b) -> f a -> f b
cofmap :: Cofunctor f => (a -> b) -> f b -> f a
-}

-- Functor

-- Applicative

-- Monad
