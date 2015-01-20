module Problem2.Proof where
import Problem2.Prod

-- I present the proof "in Haskell" just to be able to check the types
-- of the steps - this is not required.

type EqProof = []
-- Poor man's equality proofs - a list of (supposedly) equal
-- expression "proving" the equality of the first and the last
-- expression by a series of motivated steps

----------------
proof_LeftId :: (Monad m, Monad n) => a -> (a -> Prod m n b) -> EqProof (Prod m n b)
proof_LeftId a f =
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
proof_RightId :: (Monad m, Monad n) => Prod m n a -> EqProof (Prod m n a)
proof_RightId mx =
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
proof_Assoc :: (Monad m, Monad n) => Prod m n c -> (c -> Prod m n b) -> (b -> Prod m n a) -> EqProof (Prod m n a)
proof_Assoc mx f g  =
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
 , -- fstP_distributes and sndP_distributes
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

fstP_distributes :: (Monad m, Monad n) =>
   (c -> Prod m n b) -> (b -> Prod m n a) -> c -> EqProof (m a)
fstP_distributes f g x =
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

-- sndP_distributes is similar

----------------------------------------------------------------
-- Just some type checking of the laws

prop_LeftId  :: (Monad m) => (m a -> m a -> t) -> b -> (b -> m a) -> t
prop_RightId :: (Monad m) => (m a -> m a -> t) -> m a -> t
prop_Assoc   :: (Monad m) => (m a -> m a -> t) -> m c -> (c -> m b) -> (b -> m a) -> t

prop_LeftId  (~=) a f     =  ( return a >>= (\x -> f x))  ~=  ( f a                       )
prop_RightId (~=) mx      =  ( mx >>= (\x -> return x) )  ~=  ( mx                        )
prop_Assoc   (~=) mx f g  =  ( (mx >>= f) >>= g        )  ~=  (  mx >>= (\x -> f x >>= g) )
