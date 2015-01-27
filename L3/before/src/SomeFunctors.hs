module SomeFunctors where

data May a = N | J a
  deriving Show
  
instance Functor May where
  fmap = fmapMay

fmapMay :: (a->b) -> (May a -> May b)
fmapMay = error "TBD"

type Time = Double
newtype Sig a = Sig {unSig :: Time -> a}

instance Functor Sig where
  fmap = fmapSig

fmapSig :: (a->b) -> (Sig a -> Sig b)
fmapSig = error "TBD"

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap = fmapList

fmapList :: (a->b) -> (List a -> List b)
fmapList = error "TBD"

-- ----------------------------------------------------------------

instance Monad May where
  return = returnMay
  (>>=)  = bindMay

returnMay :: a -> May a
returnMay = error "TBD"

bindMay :: May a -> (a -> May b) -> May b
bindMay = error "TBD"

testMay :: May String
testMay = fmap (:[]) (J 'a')

instance Monad Sig where
  return = returnSig
  (>>=)  = bindSig

returnSig :: a -> Sig a
returnSig = error "TBD"

bindSig :: Sig a -> (a -> Sig b) -> Sig b
bindSig = error "TBD

instance Monad List where
  return = returnList
  (>>=)  = bindList

returnList :: a -> List a
returnList = error "TBD"
               
bindList :: List a -> (a -> List b) -> List b
bindList = error "TBD"





-- ----------------
-- Utility functions

(+++) :: List a -> List a -> List a
Nil        +++ ys   = ys
Cons x xs  +++ ys   = Cons x (xs +++ ys)
