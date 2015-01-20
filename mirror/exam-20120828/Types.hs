module Types where
import Control.Monad(mapM, liftM)
import Test.QuickCheck

newtype ListT m a = ListT { runListT :: m [a] }
instance (Monad m) => Monad (ListT m) where
    return  =  returnLT
    (>>=)   =  bindLT

returnLT :: Monad m => a -> ListT m a
returnLT a = ListT mLa
  where -- mLa :: m [a]
        mLa = return [a]
-- alt. the "bigmouth smiley" version
returnLT' :: Monad m => a -> ListT m a
returnLT'  = ListT . return . (:[])
-- alt. the "monad zen" version
returnLT'' :: Monad m => a -> ListT m a
returnLT'' = ListT . return . return

bindLT :: Monad m => ListT m a -> (a -> ListT m b) -> ListT m b
bindLT ltm f = ListT mLb
  where mLb = do  as <- runListT ltm
                  bss <- mapM (runListT . f) as
                  return (concat bss)

----------------------------------------------------------------
type Equal a = [a]
(===) :: a -> a -> Equal a
a === b = [a, b]
commutative :: (Monad m) => m a -> m b -> (a -> b -> m c) -> Equal (m c)
commutative m1 m2 f = lhs === rhs
  where  lhs =  do  a1 <- m1
                    a2 <- m2
                    f a1 a2 
         rhs =  do  a2 <- m2
                    a1 <- m1
                    f a1 a2 
    
-- Intermediate stage (not needed):
spec_comm_Either :: (Eq e, Eq c) => (Either e a) -> (Either e b) -> (a -> b -> Either e c) -> Equal (Either e c)
spec_comm_Either = commutative

-- Monomorphic types
type E = String
type A = Int
type B = Bool
type C = Char
spec_comm_Either_mono :: (Either E A) -> (Either E B) -> (A -> B -> Either E C) -> Equal (Either E C)
spec_comm_Either_mono = spec_comm_Either

-- the Blind modifier is not required on the exam
test_E = quickCheck (\ma mb (Blind f) -> allEq $ spec_comm_Either_mono ma mb f)

-- Only one of these three answers is needed for the exam:
-- (Either e) is not a commutative monad because bind returns the _first_ error only.
-- ((,) e) is commutative iff the Monoid e is commutative (so, not for
--   the typical case of the Writer with e = [a])
-- ((->) e) is commutative: the environment is unchanged along the computation (see below for proof - not needed on the exam)

{-
instance (Arbitrary e, Arbitrary a) => Arbitrary (Either e a) where
  arbitrary = do n <- choose (0,10)
                 if n == 0 
                   then  liftM Left   arbitrary
                   else  liftM Right  arbitrary
-}
----------------------------------------------------------------

{-
-- Available in Control.Monad.Instances
instance Monad (Either e) where
  return  =  returnE
  (>>=)   =  bindE
  
returnE :: a -> Either e a  
returnE = Right

bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left e)   f = Left e
bindE (Right a)  f = f a
-}


----------------------------------------------------------------

spec_comm_Fun_mono :: (E -> A) -> (E -> B) -> (A -> B -> E -> C) -> Equal (E -> C)
spec_comm_Fun_mono = commutative


bindF :: ((->) e) a -> (a -> ((->) e) b) -> ((->) e) b
-- bindF :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
bindF ma f = \e -> f (ma e) e
  
commute :: Monad m => m a -> m b -> (a -> b -> m c) -> Equal (m c)
commute m1 m2 f = 
  [ do a1 <- m1; a2 <- m2; f a1 a2
  , do a2 <- m2; a1 <- m1; f a1 a2
  ]

commute_F :: (e->a) -> (e->b) -> (a -> b -> (e->c)) -> Equal (e->c)
commute_F m1 m2 f = 
  [ do a1 <- m1; a2 <- m2; f a1 a2                        -- do sugar
  , m1 >>= \a1 -> do a2 <- m2; f a1 a2                    -- do sugar
  , m1 >>= \a1 -> m2 >>= \a2 -> f a1 a2                   -- (>>=) is bindF
  , bindF m1 (\a1 -> bindF m2 (\a2 -> f a1 a2))           -- Def. of bindF
  , bindF m1 (\a1 -> \e' -> (\a2 -> f a1 a2) (m2 e') e')  -- beta-reduction
  , bindF m1 (\a1 -> \e' -> f a1 (m2 e') e')              -- Def. of bindF 
  , \e -> (\a1 -> \e' -> f a1 (m2 e') e') (m1 e) e        -- beta-red.
  , \e -> (\e' -> f (m1 e) (m2 e') e') e                  -- beta-red.
  , \e -> f (m1 e) (m2 e) e                               -- Proof midpoint
  , \e -> (\e' -> f (m1 e') (m2 e) e') e                  -- ~= same backwards
  , \e -> (\a2 -> \e' -> f (m1 e') a2 e') (m2 e) e
  , bindF m2 (\a2 -> \e' -> f (m1 e') a2 e')
  , bindF m2 (\a2 -> \e' -> (\a1 -> f a1 a2) (m1 e') e')
  , bindF m2 (\a2 -> bindF m1 (\a1 -> f a1 a2))
  , m2 >>= \a2 -> m1 >>= \a1 -> f a1 a2
  , m2 >>= \a2 -> do a1 <- m1; f a1 a2
  , do a2 <- m2; a1 <- m1; f a1 a2
  ]
  
allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (x==) xs

-- the Blind modifier is not required on the exam
test3 = quickCheck (\(Blind ma) (Blind mb) (Blind f) e -> 
                     allEq $ map ($e) $ 
                     commute_F (ma :: E->A) (mb :: E->B) 
                               (f :: A -> B -> (E->C)))

