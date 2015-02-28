{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module RWMonad where
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

instance Monoid w => Monad (RW e w) where
  return = returnRW
  (>>=)  = bindRW

newtype RW e w a = RW {unRW :: () }
  deriving (Arbitrary)
returnRW :: Monoid w => a -> (RW e w) a
bindRW   :: Monoid w => (RW e w) a -> (a -> (RW e w) b) -> 
                                            (RW e w) b
askRW    :: Monoid w => (RW e w) e
--localRW  :: (e -> e) -> (RW e w) a -> (RW e w) a
tellRW   :: w -> (RW e w) ()
listenRW :: (RW e w) a -> (RW e w) (a, w)

askRW = undefined
localRW e2e (RW e2aw) = undefined
  -- e2e :: e -> e
  -- e2aw :: e -> (a, w)

returnRW a = undefined
bindRW (RW e2aw) a2m = undefined
                       
                       
-- askRW = RW $ \e -> (e, mempty)
-- localRW f (RW e2aw) = RW $ \e -> e2aw (f e)
tellRW w = undefined
listenRW (RW e2aw) = undefined
                     

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
  ,
    f x
  ]

proofBindReturn m = AllEq
  [
    m >>= return 
  ,
    m
  ]

proofBindBind m f g  = AllEq
  [
    (m >>= f) >>= g
  ,
    (m >>= (\x-> f x >>= g))
  ]
{-
proofReturnBind :: (Monoid w) => a -> (a -> RW e w b) -> 
                   AllEq (RW e w b)
proofReturnBind x f = AllEq 
  [
    (return x >>= f)
  , -- Monad instance for RW
    (returnRW x `bindRW` f)
  , {- def. returnRW -}
    ((RW $ \e->(x, mempty)) `bindRW` f)
  , {- def. bindRW -}
    let e2aw = \e->(x, mempty) 
        a2m = f
    in RW $ \e -> let (a, w1) = e2aw e
                      (b, w2) = unRW (a2m a) e
                  in (b, w1 `mappend` w2)
  , {- inlining -}
    RW $ \e -> let (a, w1) = (\e->(x, mempty)) e
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
                   (b, w2) = unRW (RW $ \e->(a, mempty)) e
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
                     in RW $ \e -> let (a, w1) = e2aw e
                                       (b, w2) = unRW (a2m a) e
                                   in (b, w1 `mappend` w2)
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

mytest (AllEq ms) = forAll arbitrary $ \e -> 
                    allEq $ map (flip unRW e) ms

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



-}