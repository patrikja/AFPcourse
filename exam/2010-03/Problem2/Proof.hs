module Problem2.Proof where
import Problem2.Prod

-- I present the proof "in Haskell" just to be able to check the types
-- of the steps - this is not required.

type EqProof = []
-- Poor man's equality proofs - a list of (supposedly) equal
-- expression "proving" the equality of the first and the last
-- expression by a series of motivated steps

----------------
proofLeftId :: (Monad m, Monad n) => a -> (a -> Prod m n b) -> EqProof (Prod m n b)
proofLeftId a f =
  [ return a >>= (\x -> f x)
  , -- def. of return
    Prod (return a, return a) >>= (\x -> f x)
  , -- def. of (>>=)
    Prod ( return a >>= (fstP . f)
         , return a >>= (sndP . f) )
  , -- LeftId law for m and for n
    Prod ( (fstP . f) a
         , (sndP . f) a )
  , -- def. of composition
    Prod ( fstP (f a)
         , sndP (f a) )
  , -- SurjectivePairing
    Prod ( unProd (f a) )
  , -- Inverses
    f a
  ]

----------------
proofRightId :: (Monad m, Monad n) => Prod m n a -> EqProof (Prod m n a)
proofRightId mx =
  [ mx >>= return
  , -- inverses
    Prod (unProd mx) >>= return
  , -- surjectivePairing
    Prod (fstP mx, sndP mx) >>= return
  , -- def. of (>>=), return
{- -- commented out due to ambiguous type variables
    Prod ( fstP mx >>= (fstP . returnProd)
         , sndP mx >>= (sndP . returnProd) )
  , -- fstP_return and sndP_return
-}
    Prod ( fstP mx >>= return
         , sndP mx >>= return )
  , -- RightId law for m and for n
    Prod ( fstP mx
         , sndP mx )
  , -- surjectivePairing
    Prod (unProd mx)
  , -- Inverses
    mx
  ]

{-
-- commented out due to ambiguous type variables

fstP_return x =
  [ (fstP . returnProd) x
  , -- def. of (.)
    fst (unProd (returnProd x))
  , -- def. of returnProd
    fst (unProd (Prod (return x, return x)))
  , -- Inverses
    fst (return x, return x)
  , -- def. of fst
    return x
  ]

sndP_return :: (Monad n) => a -> n a
sndP_return x =
  [ (sndP . returnProd) x
  , -- as in fstP_return
    return x
  ]
-}

-- ----------------------------------------------------------------
proofAssoc :: (Monad m, Monad n) => Prod m n c -> (c -> Prod m n b) -> (b -> Prod m n a) -> EqProof (Prod m n a)
proofAssoc mx f g  =
 [ (mx >>= f) >>= g
 , -- Inverses, surjectivePairing
   (Prod (fstP mx, sndP mx) >>= f) >>= g
 , -- Def. of (>>=)
   (Prod ( fstP mx >>= (fstP . f)
         , sndP mx >>= (sndP . f) ) ) >>= g
 , -- Def. of (>>=)
   Prod ( (fstP mx >>= (fstP . f)) >>= (fstP . g)
        , (sndP mx >>= (sndP . f)) >>= (sndP . g))
 , -- Assoc. law for the underlying monads m and n
   Prod ( fstP mx >>= \x -> (fstP . f) x >>= (fstP . g)
        , sndP mx >>= \y -> (sndP . f) y >>= (sndP . g) )
 , -- Def. of (.)
   Prod ( fstP mx >>= \x -> fstP (f x) >>= (fstP . g)
        , sndP mx >>= \y -> sndP (f y) >>= (sndP . g) )
 , -- fstPDistributes and sndPDistributes
   Prod ( fstP mx >>= \x -> fstP (f x >>= g)
        , sndP mx >>= \y -> sndP (f y >>= g) )
 , -- Def. of (.) + alpha renaming
   Prod ( fstP mx >>= (fstP . (\x -> f x >>= g))
        , sndP mx >>= (sndP . (\x -> f x >>= g)) )
 , -- def. of (>>=)
   Prod (fstP mx, sndP mx) >>= (\x -> f x >>= g)
 , -- def. of return
   Prod (fstP mx, sndP mx) >>= (\x -> f x >>= g)
 , -- expand mx
   mx >>= (\x -> f x >>= g)
 ]

----------------------------------------------------------------
-- Not part of the exam question

fstPDistributes :: (Monad m, Monad n) =>
   (c -> Prod m n b) -> (b -> Prod m n a) -> c -> EqProof (m a)
fstPDistributes f g x =
 [ fstP (f x) >>= (fstP . g)
 , -- Def. of fstP
   fst (unProd (f x)) >>= (fstP . g)
 , -- Def. of fst
   fst ( fst (unProd (f x)) >>= (fstP . g)
       , snd (unProd (f x)) >>= (sndP . g) )
 , -- Inverses
   fst (unProd (Prod ( fst (unProd (f x)) >>= (fstP . g)
                     , snd (unProd (f x)) >>= (sndP . g) )))
 , -- def. of (>>=), fstP
   fstP (Prod (fst (unProd (f x)), snd (unProd (f x))) >>= g)
 , -- expand (f x)
   fstP (f x >>= g)
 ]

-- sndPDistributes is similar

----------------------------------------------------------------
-- Just some type checking of the laws

propLeftId  :: (Monad m) => (m a -> m a -> t) -> b -> (b -> m a) -> t
propRightId :: (Monad m) => (m a -> m a -> t) -> m a -> t
propAssoc   :: (Monad m) => (m a -> m a -> t) -> m c -> (c -> m b) -> (b -> m a) -> t

propLeftId  (~=) a f     =  ( return a >>= (\x -> f x))  ~=  ( f a                       )
propRightId (~=) mx      =  ( mx >>= (\x -> return x) )  ~=  ( mx                        )
propAssoc   (~=) mx f g  =  ( (mx >>= f) >>= g        )  ~=  (  mx >>= (\x -> f x >>= g) )
