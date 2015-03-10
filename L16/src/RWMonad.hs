{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module RWMonad where
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
-- import qualified Test.QuickCheck.Property as QC
-- import qualified Test.QuickCheck.Arbitrary as QC

{-

Fake exam question:

A. Implement a "Read-Write" monad |m = RW e w| (for any type |e|
and and monoid |w|) with the following operations:

\begin{code}  
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  ask    :: m e
  local  :: (e -> e) -> m a -> m a
  tell   :: w -> m ()
  listen :: m a -> m (a, w)
\end{code}

B. State and prove the three Monad laws for |RW e w|.

-}

instance Functor (RW e w) where
  fmap = fmapRW

fmapRW :: (a -> b) -> RW e w a -> RW e w b
fmapRW f (RW g) = RW $ \e -> let (a, w) = g e in (f a, w)
  
instance Monoid w => Applicative (RW e w) where
  pure = pureRW
  (<*>) = apRW
  
pureRW :: Monoid w => a -> RW e w a
pureRW a = RW $ \_e-> (a, mempty)

apRW :: Monoid w => RW e w (a -> b) -> RW e w a -> RW e w b
apRW (RW gf) (RW ga) = RW $ \e -> let (f, wf) = gf e
                                      (a, wa) = ga e
                                  in  (f a, wf `mappend` wa)

instance Monoid w => Monad (RW e w) where
  return = returnRW   -- = pureRW
  (>>=)  = bindRW

newtype RW e w a = RW {unRW :: e -> (a, w)}
  deriving (Arbitrary)
returnRW :: Monoid w => a -> (RW e w) a
bindRW   :: Monoid w => (RW e w) a -> (a -> (RW e w) b) -> 
                                            (RW e w) b
askRW    :: Monoid w => (RW e w) e
--localRW  :: (e -> e) -> (RW e w) a -> (RW e w) a
tellRW   :: w -> (RW e w) ()
listenRW :: (RW e w) a -> (RW e w) (a, w)

askRW = RW (\e -> (e, mempty))
localRW :: (e1 -> e2) -> RW e2 w a -> RW e1 w a
localRW e2e (RW e2aw) = RW $ \ e -> e2aw (e2e e)








returnRW = pureRW
bindRW (RW e2aw) a2m = RW $ \e -> let (a, w1) = e2aw e
                                      (b, w2) = unRW (a2m a) e
                                  in (b, w1 `mappend` w2)
-- askRW = RW $ \e -> (e, mempty)
-- localRW f (RW e2aw) = RW $ \e -> e2aw (f e)
tellRW w = RW $ \_e -> ((), w)
listenRW (RW e2aw) = RW $ \e -> let (a, w) = e2aw e
                                in ((a, w), w)

-- State and prove the three Monad laws

lawReturnBind :: (Eq (m b), Monad m) => a -> (a -> m b) -> Bool
lawReturnBind x f  =  (return x >>= f)  ==  f x

lawBindReturn :: (Eq (m b), Monad m) => m b -> Bool
lawBindReturn m    =  (m >>= return)    ==  m

lawBindBind ::
  (Eq (m c), Monad m) => m a -> (a -> m b) -> (b -> m c) -> Bool
lawBindBind m f g  = ((m >>= f) >>= g) ==  (m >>= (\x-> f x >>= g))

-- Straw-man proofs in Haskell (just a list of supposedly equal
-- expr.)
newtype AllEq a = AllEq [a] 
  deriving (Arbitrary)

proofReturnBind :: (Monoid w) => a -> (a -> RW e w b) -> 
                   AllEq (RW e w b)
proofReturnBind x f = AllEq 
  [
    (return x >>= f)
  , -- Monad instance for RW
    (returnRW x `bindRW` f)
  , {- def. returnRW -}
    ((RW $ \_e->(x, mempty)) `bindRW` f)
  , {- def. bindRW -}
    let e2aw = \_e->(x, mempty) 
        a2m = f
    in RW $ \e -> let (a, w1) = e2aw e
                      (b, w2) = unRW (a2m a) e
                  in (b, w1 `mappend` w2)
  , {- inlining -}
    RW $ \e -> let (a, w1) = (\_e->(x, mempty)) e
                   (b, w2) = unRW (f a) e
               in (b, w1 `mappend` w2)
  , {- beta-red.  -}
    RW $ \e -> let (a, w1) = (x, mempty)
                   (b, w2) = unRW (f a) e
               in (b, w1 `mappend` w2)
  , {- inlining a, w1 -}
    RW $ \e -> let (b, w2) = unRW (f x) e
               in (b, mempty `mappend` w2)
  , {- Monoid law -}
    RW $ \e -> let (b, w2) = unRW (f x) e
               in (b, w2)
  , {- inlining -}
    RW $ \e -> unRW (f x) e
  , {- eta-reduction -}
    RW $ unRW (f x)
  , {- RW . unRW == id -}
    f x
  ]

proofBindReturn :: Monoid w => RW e w a -> AllEq (RW e w a)
proofBindReturn m = AllEq
  [
    m >>= return 
  , {- type instance -}
    m `bindRW` returnRW
  , {- def. of bindRW -}
    let e2aw = unRW m
        a2m  = returnRW
    in RW $ \e -> let (a, w1) = e2aw e
                      (b, w2) = unRW (a2m a) e
                  in (b, w1 `mappend` w2)
  , {- inlining -}
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = unRW (returnRW a) e
               in (b, w1 `mappend` w2)
  , {- def. returnRW -}
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = unRW (RW $ \_e->(a, mempty)) e
               in (b, w1 `mappend` w2)
  , {- unRW . RW == id, beta-red. -}
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = (a, mempty)
               in (b, w1 `mappend` w2)
  , {- inlining -}
    RW $ \e -> let (a, w1) = unRW m e
               in (a, w1 `mappend` mempty)
  , {- Monoid law -}
    RW $ \e -> let (a, w1) = unRW m e
               in (a, w1)
  , {- inlining -}
    RW $ \e -> unRW m e
  , {- eta-contraction, unRW . RW == id -}
    m
  ]



proofBindBind :: Monoid w =>
                 RW e w a1 -> (a1 -> RW e w a2) -> (a2 -> RW e w a3) ->
                 AllEq (RW e w a3)
proofBindBind m f g  = AllEq
  [
    (m >>= f) >>= g
  , {- Monad instance-}  
    (m `bindRW` f) `bindRW` g
  , -- def. bindRW
    let e2aw = unRW (m `bindRW` f)
        a2m  = g
    in RW $ \e -> let (a, w1) = e2aw e
                      (b, w2) = unRW (a2m a) e
                  in (b, w1 `mappend` w2)
  , -- inlining
    RW $ \e -> let (a, w1) = unRW (m `bindRW` f) e
                   (b, w2) = unRW (g a) e
               in (b, w1 `mappend` w2)
  , -- def. bindRW (again) + inline + prime, unRW . RW == id, beta
    RW $ \e -> let (a, w1) = let (a', w1') = unRW m e
                                 (b', w2') = unRW (f a') e
                             in (b', w1' `mappend` w2')
                   (b, w2) = unRW (g a) e
               in (b, w1 `mappend` w2)

  , -- let-float
    RW $ \e -> let (a', w1') = unRW m e     
                   (b', w2') = unRW (f a') e
                   (a, w1) = (b', w1' `mappend` w2')
                   (b, w2) = unRW (g a) e
               in (b, w1 `mappend` w2)
  , -- substitute
    RW $ \e -> let (a', w1') = unRW m e     
                   (b', w2') = unRW (f a') e
                   (b, w2)   = unRW (g b') e
               in (b, (w1' `mappend` w2') `mappend` w2)

  ,  -- rename + Monoid append associativity
    RW $ \e -> let (a, w1)   = unRW m e     
                   (b, w2)   = unRW (f a) e
                   (c, w3)   = unRW (g b) e
               in (c, w1 `mappend` (w2 `mappend` w3))
  ,
    RW $ \e -> let (a, w1)   = unRW m e
                   (a', w1') = unRW (f a) e
                   (b', w2') = unRW (g a') e
               in (b', w1 `mappend` (w1' `mappend` w2'))
  ,  -- substitute
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = let (a', w1') = unRW (f a) e
                                 (b', w2') = unRW (g a') e
                             in (b', w1' `mappend` w2')
               in (b, w1 `mappend` w2)
  ,  -- inline (+add primes), unRW . RW == id, beta red.
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = unRW (
                     let e2aw = unRW (f a)
                         a2m  = g
                     in RW $ \e' -> let (a', w1') = e2aw e'
                                        (b', w2') = unRW (a2m a') e'
                                    in (b', w1' `mappend` w2')
                     ) e
               in (b, w1 `mappend` w2)
  ,  -- def. of bindRW (again)
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = unRW (f a `bindRW` g) e
               in (b, w1 `mappend` w2)
  ,  -- beta-red.
    RW $ \e -> let (a, w1) = unRW m e
                   (b, w2) = unRW ((\x-> f x `bindRW` g) a) e
               in (b, w1 `mappend` w2)
  ,  -- inlining
    let e2aw = unRW m
        a2m  = (\x-> f x `bindRW` g)
    in RW $ \e -> let (a, w1) = e2aw e
                      (b, w2) = unRW (a2m a) e
                  in (b, w1 `mappend` w2)
  , -- def. bindRW
    (m `bindRW` (\x-> f x `bindRW` g))
  , {- Monad instance-}  
    (m >>= (\x-> f x >>= g))
  ]

mytest :: (Arbitrary b, Show b, Eq a, Eq w) =>
          AllEq (RW b w a) -> Property
mytest (AllEq ms) = forAll arbitrary $ \e -> 
                    allEq $ map (flip unRW e) ms

allEq :: Eq a => [a] -> Bool
allEq []      =  True
allEq (x:xs)  =  all (x==) xs

instance (Eq a, Eq w, Show b, Arbitrary b) => 
         Testable (AllEq (RW b w a)) where 
  property = mytest

proofReturnBind' :: Bool -> Blind (Bool -> RW Int String Char) -> 
                    AllEq (RW Int String Char)
proofReturnBind' x (Blind f) = proofReturnBind x f

proofBindReturn' :: Blind (RW Int String Char) -> 
                    AllEq (RW Int String Char)
proofBindReturn' (Blind m) = proofBindReturn m

type RWIS = RW Int String

proofBindBind' :: Blind (RWIS Bool) -> 
                  Blind (Bool -> RWIS Char) -> 
                  Blind (Char -> RWIS Int) -> 
                  AllEq (RWIS Int)
proofBindBind' (Blind m) (Blind f) (Blind g) = proofBindBind m f g
  
main :: IO ()
main = do 
  quickCheck proofBindBind'
  quickCheck proofBindReturn'
  quickCheck proofReturnBind'


-- ----------------------------------------------------------------
{-
instance Eq a => Testable (AllEq a) where
  property = propertyAllEq 
                          
propertyAllEq :: Eq a => AllEq a -> Property
propertyAllEq (AllEq xs) = QC.property $ QC.liftBool $ allEq xs
-}

{-
instance (Arbitrary e, Show e, Testable prop) => 
         Testable (RW e w a) where
  property f = forAllShrink arbitrary shrink f
-}



