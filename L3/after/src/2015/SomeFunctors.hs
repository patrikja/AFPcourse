module SomeFunctors where

data May a = N | J a
  deriving Show
  
instance Functor May where
  fmap = fmapMay

instance Monad May where
  return = returnMay
  (>>=)  = bindMay

returnMay :: a -> May a
returnMay x = J x

bindMay :: May a -> (a -> May b) -> May b
bindMay N     _g = N
bindMay (J x)  g = g x

fmapMay :: (a->b) -> (May a -> May b)
fmapMay _f   N     = N
fmapMay f   (J x)  = J (f x)

testMay :: May String
testMay = fmap (:[]) (J 'a')

type Time = Double
newtype Sig a = Sig {unSig :: Time -> a}

instance Functor Sig where
  fmap = fmapSig

fmapSig :: (a->b) -> (Sig a -> Sig b)
fmapSig f (Sig s) = Sig (f . s)
-- fmapSig f (Sig s) = Sig (\t -> f (s t))
  -- s :: Time -> a
  -- q :: Sig b
  -- ? :: Time -> b                  

instance Monad Sig where
  return = returnSig
  (>>=)  = bindSig

returnSig :: a -> Sig a
returnSig x = Sig (\_t -> x)

bindSig :: Sig a -> (a -> Sig b) -> Sig b
bindSig (Sig sa) g = Sig $ \t-> let sb = unSig (g (sa t)) in sb t
-- bindSig (Sig sa) g = \t-> (g (sa t)) t
--                      \t -> g (f t)
  -- sa :: Time -> a
  -- sa t :: a
  -- g (sa t) :: Sig b   
  -- sb :: Time -> b


data List a = Nil | Cons a (List a)

instance Functor List where
  fmap = fmapList

fmapList :: (a->b) -> (List a -> List b)
fmapList _f Nil          = Nil
fmapList f  (Cons x xs)  = Cons (f x) (fmapList f xs) 

instance Monad List where
  return = returnList

  (>>=)  = bindList

returnList :: a -> List a
returnList x = Cons x Nil
  -- Cons x (Cons x Nil)
               
bindList :: List a -> (a -> List b) -> List b
bindList Nil          _g  = Nil
bindList (Cons x xs)   g  = firstPart +++ rest
  where firstPart = g x            -- List b 
        rest      = bindList xs g  -- List b

(+++) :: List a -> List a -> List a
(+++) = undefined
