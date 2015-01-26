module SomeFunctors where

data May a = N | J a

instance Functor May where
  fmap = fmapMay

fmapMay :: (a->b) -> (May a -> May b)
fmapMay _ N      =  N
fmapMay f (J x)  =  J (f x)

type Time = Double
newtype Sig a = Sig {unSig :: Time -> a}

instance Functor Sig where
  fmap = fmapSig

fmapSig :: (a->b) -> (Sig a -> Sig b)
fmapSig f (Sig s) = Sig $ \t -> f (s t)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap = fmapList

fmapList :: (a->b) -> (List a -> List b)
fmapList _ Nil         = Nil
fmapList f (Cons x xs) = Cons (f x) (fmap f xs)


