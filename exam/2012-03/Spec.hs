{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spec where
import qualified Control.Monad.State as CMS
import Control.Monad.State (MonadState, get, put)
import Control.Applicative -- not required for the exam
import Test.QuickCheck -- not needed for the exam question
import qualified Test.QuickCheck.Property as QC
import qualified Test.QuickCheck.Arbitrary as QC

------
-- Exam question, supporting code
-- The four MonadState laws (specialised to (S2 s a) to avoid ambiguous types):
putput :: Eq s => s -> s -> s -> Bool
putput s' s = 
  (put s' >> put s)                      =.=  put s

putget :: Eq s => s -> s -> Bool
putget s = 
  (put s >> get)                         =.=  (put s >> return s)

getput :: Eq s => S2 s () -> s -> Bool
getput phony = -- the first argument is only to disambiguate the types
  (get >>= put)                          =.=  (skip `asTypeOf` phony)

getget :: (Eq a, Eq s) => (s -> s -> S2 s a) -> s -> Bool
getget k = 
  (get >>= \s -> get >>= \s' -> k s s')  =.=  (get >>= \s -> k s s)

skip :: Monad m => m ()
skip = return ()

-- Consider the following instance:
data S2 s a where
  Return  ::  a -> S2 s a
  Bind    ::  S2 s a -> (a -> S2 s b) -> S2 s b
  Then    ::  S2 s a -> S2 s b -> S2 s b
  Get     ::  S2 s s
  Put     ::  s -> S2 s ()

instance Monad (S2 s) where {return = Return; (>>=) = Bind; (>>) = Then}
instance MonadState s (S2 s) where {get = Get; put = Put}

{- Task a:  
  Implement a run function |runS2 :: S2 s a -> (s -> (a, s))| and
  prove (by equational reasoning) that the put-put and put-get laws
  hold if |(==)| means ``all runs are equal''.
-}

-- Run function (part of the answer to Task a):

runS2 :: S2 s a   -> (s -> (a, s))
runS2 (Return a)  =  \s -> (a, s)
runS2 (Bind m f)  =  \s -> let (a, s') = runS2 m s  in runS2 (f a) s'
runS2 (Then m m') =  \s -> let (_, s') = runS2 m s  in runS2 m' s'
runS2 Get         =  \s -> (s,  s)
runS2 (Put s)     =  \_ -> ((), s)

{- For the proof part of Task a we apply runS2 to the lhs and rhs in
the same starting state and show equality by equational reasoning.
For testing and type-checking purposes I write the steps of the proof
in a Haskell list - this is not required in the exam.  -}

-- Proof of putput s' s =  (put s' >> put s) =.=  put s
putput_proof :: s -> s -> s -> AllEq ((), s)
putput_proof s1 s2 s3 = AllEq  
  [ runS2 (put s1 >> put s2) s3
  ,  -- def. of put, (>>)
    runS2 (Then (Put s1) (Put s2)) s3
  ,  -- def. of runS2 for Then
    (\s -> let (_, s') = runS2 (Put s1) s  in runS2 (Put s2) s') s3
  ,  -- def. of runS2 for Put (twice)
    (\s -> let (_, s') = ((),s1) in ((),s2)) s3
  ,  -- beta-red. & simplify unreachable binding
    ((), s2)
  ,  -- beta-red. (from here all steps are "backwards")
    (\_ -> ((), s2)) s3
  ,  -- def. of runS2 for Put
    runS2 (Put s2) s3
  ,  -- def. of put
    runS2 (put s2) s3
  ]

-- Proof of put-get:  (put s >> get) ==  (put s >> return s)
putget_proof :: Eq s => s -> s -> AllEq (s, s)
putget_proof s1 s2 = AllEq
  [ runS2 (put s1 >> get) s2
  , -- def. of put, get, (>>)
    runS2 (Then (Put s1) Get) s2
  , -- def. of runS2 for Then
    (\s -> let (_, s') = runS2 (Put s1) s  in runS2 Get s') s2
  , -- def. of runS2 for Put and beta-red.
    let (_, s') = ((), s1)  in runS2 Get s'
  , -- inline
    runS2 Get s1
  , -- def. of runS2 for Get and beta-red.
    (s1, s1)
  , -- def. of runS2 for Return and beta-red.
    runS2 (Return s1) s1
  , -- inline
    let (_, s') = ((), s1)  in runS2 (Return s1) s'
  , -- def. of runS2 for Put and beta-red.
    (\s -> let (_, s') = runS2 (Put s1) s  in runS2 (Return s1) s') s2
  , -- def. of runS2 for Then
    runS2 (Then (Put s1) (Return s1)) s2
  , -- def. put, return, (>>)
    runS2 (put s1 >> return s1) s2
  ]

-- Task (b): Program transformation based on reasoning

data S3 s a where
  Ret3     ::  a -> S3 s a
  GetBind  ::  (s -> S3 s a)  ->  S3 s a
  PutThen  ::  s -> S3 s a    ->  S3 s a

opt :: S2 s a -> S3 s a
opt (Return a)    =  Ret3 a
opt Get           =  get3
opt (Put s)       =  put3 s
opt (Bind m f)    =  removeBind m f
opt (Then m n)    =  removeThen m n

put3 :: s -> S3 s ()
put3 s = PutThen s (Ret3 ())

get3 :: S3 s s
get3 = GetBind Ret3  

-- Task: Implement |removeBind| and |removeThen| and motivate your definitions.
removeBind :: S2 s a -> (a -> S2 s b) -> S3 s b
removeBind (Return a)  f  =  opt (f a)                        -- Monad law 1
removeBind Get         f  =  GetBind (opt . f)                -- new constructor
removeBind (Put s)     f  =  PutThen s (opt (f ()))           -- new constructor
removeBind (Then m n)  f  =  opt (Then m (Bind n f))          -- Monad law 3'
removeBind (Bind m f)  g  =  opt (Bind m (\a-> Bind (f a) g)) -- Monad law 3

removeThen :: S2 s a -> S2 s b -> S3 s b
removeThen (Return a)  n  =  opt n                            -- Monad law 1'
removeThen Get         n  =  opt n                            -- prop. of Get
removeThen (Put s)     n  =  PutThen s (opt n)                -- new constructor
removeThen (Then m n)  o  =  opt (Then m (Then n o))          -- Monad law 3''
removeThen (Bind m f)  n  =  opt (Bind m (\a-> Then (f a) n)) -- Monad law 3'''


{-
-- Alternative right-hand-sides for some of the cases:
removeBind (Then m n)  f  =  removeThen m (Bind n f)
removeBind (Bind m f)  g  =  removeBind m (\a-> Bind (f a) g)

removeThen (Then m n)  o  =  removeThen m (Then n o)
removeThen (Bind m f)  n  =  removeBind m (\a-> Then (f a) n)
-}

removeBindThen ::
  S2 s a1 -> S2 s a2 -> (a2 -> S2 s a) -> AllEq (S3 s a)
removeBindThen m n f = AllEq
  [ removeBind (Then m n) f  
  , -- def. of opt for Bind
    opt (Bind (Then m n) f)
  , -- Monad law 3 (specialised for Then (>>))
    opt (Then m (Bind n f))         
  , -- def. of opt for Then
    removeThen m (Bind n f)
  ]

removeBindBind m f g = AllEq
  [ removeBind (Bind m f) g
  , -- def. opt for Bind
    opt (Bind (Bind m f) g)
  , -- Monad law 3
    opt (Bind m (\a-> Bind (f a) g))
  , -- def. of opt for Bind
    removeBind m (\a-> Bind (f a) g)
  ]

removeThenThen m n o = AllEq
  [ removeThen (Then m n)  o  
  , -- Monad law 3''
    opt (Then m (Then n o))
  , -- def. of opt for Then
    removeThen m (Then n o)
  ]

removeThenBind m f n = AllEq
  [ removeThen (Bind m f)  n  
  , -- Monad law 3'''
    opt (Bind m (\a-> Then (f a) n))
  , -- def. of opt for Bind
    removeBind m (\a-> Then (f a) n)
  ]

-- --------------
-- Below is some supporting code to sanity-check the exam answers.

runS3 :: S3 s a      -> (s -> (a, s))
runS3 (Ret3 a)       =  \s -> (a, s) 
runS3 (GetBind f)    =  \s -> runS3 (f s) s
runS3 (PutThen s m)  =  \_ -> runS3 m s

saneOpt m = \s -> runS2 m s == runS3 (opt m) s

testOpt = quickCheck (saneOpt . compileMState)

-- examples:
inp1 = MThen (MPut True) (MBind (MThen (MPut True) (MReturn False)) (A2MState (MThen (MPut True) (MReturn False)) (MReturn True)))
inp2 = MThen (MPut False) (MReturn True)

-- S3 s is also a state monad
instance Monad (S3 s) where {return = Ret3; (>>=) = bind3}
instance MonadState s (S3 s) where {put = put3; get = get3}

bind3 :: S3 s a -> (a -> S3 s b) -> S3 s b
bind3 (Ret3 a)       f  =  f a
bind3 (GetBind f)    g  =  GetBind (\s-> bind3 (f s) g)
bind3 (PutThen s m)  f  =  PutThen s (bind3 m f)
  

-- Now, how can we use the put-* laws?
opt3 :: S3 s a -> S3 s a  
opt3 (PutThen _ (PutThen s m))  =  PutThen s m  -- put-put
opt3 (PutThen s (GetBind f))    =  PutThen s (opt3 (f s)) -- put-get

saneOpt3 m = \s -> runS3 m' s == runS3 (opt3 m') s
  where m' = opt m

testOpt3 = quickCheck (saneOpt . compileMState)

-- --------------




-- --------------------------------------------------------------
-- Equality check:
(=.=) :: (Eq a, Eq s) => S2 s a -> S2 s a -> s -> Bool
m =.= n = \s -> runS2 m s == runS2 n s

type S = Bool
getget' :: Fun -> S -> Bool
getget' (Fun fun) = getget (\s s' -> compileMState (fun s s'))

putput' :: S -> S -> S -> Bool
putput' = putput

putget' :: S -> S -> Bool
putget' = putget

getput' :: S -> Bool
getput' = getput (undefined :: S2 S ())
  
test1 = do quickCheck getget'
           quickCheck putput'
           quickCheck putget'
           quickCheck getput'

newtype Fun = Fun {unFun :: S -> S -> MState}
  deriving (Arbitrary)

instance Show Fun where
  show = showFun
  
showFun (Fun f) = concatMap show [f x y | x <- [False, True], y <- [False, True]]

instance (Bounded s, Enum s, Show s, Show a) => Show (S2 s a) where
  show = showS2

showS2 sm = concatMap show [g s | s <- [minBound..maxBound]]
  where g = runS2 sm
        



{-
-- A polymorphic instance does not work with the GADT S2
instance (Arbitrary s, Arbitrary a) => Arbitrary (S2 s a) where
  arbitrary = arbitraryS2
  
arbitraryS2 :: (Arbitrary s, Arbitrary a) => Gen (S2 s a)
arbitraryS2 = oneof 
  [ Return <$> arbitrary
  , Bind   <$> arbitrary <*> arbitrary  -- ambiguous
  , Then   <$> arbitrary <*> arbitrary  -- ambiguous
  , pure Get                            -- type mismatch
  , Put    <$> arbitrary                -- type mismatch
  ]
-}

type A = Bool
data MState where -- a result-monomorphic version of S2 s a
  MReturn  ::  A -> MState
  MBind    ::  MState -> (A2MState) -> MState
  MThen    ::  MState' -> MState -> MState
  MGet     ::  MState
  deriving Show

data MState' where
  MPut     ::  S -> MState'
  deriving Show

compileMState' :: MState' -> S2 S ()
compileMState' (MPut s)     =  Put s


data A2MState = A2MState MState MState -- False and True cases
  deriving Show  

compileMState :: MState -> S2 S A
compileMState (MReturn a)  =  Return a
compileMState (MBind m f)  =  Bind (compileMState m) (compileA2MState f)
compileMState (MThen m n)  =  Then (compileMState' m) (compileMState n)
compileMState (MGet)       =  Get
  
compileA2MState :: A2MState -> (A -> S2 S A)
compileA2MState (A2MState f t) a = compileMState $ if a then t else f

instance Arbitrary MState where
  arbitrary = arbitraryMState
  
instance Arbitrary MState' where
  arbitrary = arbitraryMState'

instance Arbitrary A2MState where
  arbitrary = arbitraryA2MState

arbitraryMState' :: Gen MState'
arbitraryMState' = MPut <$> arbitrary

arbitraryMState :: Gen MState
arbitraryMState = oneof 
  [ MReturn <$> arbitrary
  , MBind   <$> arbitrary <*> arbitrary
  , MThen   <$> arbitrary <*> arbitrary
  , pure MGet                            -- type cheat - state S = result type A
  ]

arbitraryA2MState :: Gen A2MState
arbitraryA2MState = A2MState <$> arbitrary <*> arbitrary


----------------------------------------------------------------
-- Sanity checking of the "proofs" - not needed on the exam:

-- Straw-man proofs in Haskell (just a list of supposedly equal expr.)
newtype AllEq a = AllEq [a] 
  deriving (Arbitrary)
{- 
mytest (AllEq ms) = forAll arbitrary $ \e -> 
                    allEq $ map (flip runS2 e) ms

instance (Arbitrary s, Show s, Eq a, Eq s) =>
         Testable (AllEq (S2 s a)) where 
  property = mytest
-}

instance Eq a => Testable (AllEq a) where
  property = propertyAllEq 
                          
propertyAllEq :: Eq a => AllEq a -> Property
propertyAllEq (AllEq xs) = QC.property $ QC.liftBool $ allEq xs
    
allEq []      =  True
allEq (x:xs)  =  all (x==) xs

    

test2 = do quickCheck (\s -> putput_proof (s :: Int))
           quickCheck (\s -> putget_proof (s :: Int))

main = test1 >> test2
