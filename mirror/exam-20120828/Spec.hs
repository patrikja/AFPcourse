module Spec where

-- a) 
{-
-- These are already in Control.Monad.Instances
instance Functor (Either e)  where  fmap = fmapE
instance Functor ((,) e)     where  fmap = fmapP
instance Functor ((->) e)    where  fmap = fmapF
-}
  
fmapE :: (a->b) -> (Either e a) -> (Either e b)
fmapE f  (Left e)   =  Left e
fmapE f  (Right a)  =  Right (f a)
  
fmapP :: (a->b) -> (e, a) -> (e, b)  
fmapP f  (e, a)     =  (e, f a)
  
fmapF :: (a->b) -> (e->a) -> (e->b)
fmapF f  g          =  f . g

{-
-- Monads?

(Either e) is the Error monad
((,) e)    is the Writer monad if e is a monoid (less well-known)
((->) e)   is the Reader monad

-}

----------------------------------------------------------------
-- b)


-- --------------

-- Functor laws (here as QuickCheck properties)
fmapId :: (Functor f, Eq (f a)) => f a -> Bool
fmapId        =  fmap id === id
fmapComp f g  =  (fmap f) . (fmap g)  ===  fmap (f . g)

infix 4 ===
(===) :: Eq a => (t -> a) -> (t -> a) -> t -> Bool
(f === g) x  =  f x == g x

-- Prove the Functor laws

-- fmapId

fmapId_E x = 
    [ fmapE id x
    , case x of
        Left e  -> Left e
        Right a -> Right (id a)
    , case x of
        Left e  -> Left e
        Right a -> Right a    
    , x
    ]

fmapId_P (e, a) = 
    [ fmapP id (e, a)
    , (e, id a)
    , (e, a)
    ]
    
fmapId_F f =    
  [ fmapF id f
  , id . f
  , \x -> id (f x)
  , \x -> f x
  ]          
  
----------------
-- fmapComp

fmapComp_E f g (Left e) =  
  [ ((fmapE f) . (fmapE g)) (Left e)
  , fmapE f (fmapE g (Left e))
  , fmapE f (Left e)
  , Left e
  , fmapE (f . g) (Left e)
  ] 
fmapComp_E f g (Right a) =  
  [ ((fmapE f) . (fmapE g)) (Right a)
  , fmapE f (fmapE g (Right a))
  , fmapE f (Right (g a))
  , Right (f (g a))
  , Right ((f . g) a)
  , fmapE (f . g) (Right a)
  ]

fmapComp_P f g (e, a) =
  [ ((fmapP f) . (fmapP g)) (e, a)
  , fmapP f (fmapP g (e, a))
  , fmapP f (e, g a)
  , (e, f (g a))
  , (e, (f . g) a)
  , fmapP (f . g) (e, a)
  ]
  
fmapComp_F f g h =
  [ ((fmapF f) . (fmapF g)) h
  , fmapF f (fmapF g h)
  , fmapF f (g . h)
  , f . (g . h)
  , (f . g) . h
  , fmapF (f . g) h
  ]


----------------
-- Some testing code (just started) - not part of the exam question.

allEq :: Eq a => [a] -> Bool
allEq []      =  True
allEq (x:xs)  =  all (x==) xs

p_E f g x = allEq (fmapComp_E f g x)

