{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Types where
import Prelude hiding (lookup)
import Control.Monad ((>=>))

-- Code from the exam question:
class GMapKey k where
  data GMap k  :: * -> *
  empty        :: GMap k v
  lookup       :: k -> GMap k v -> Maybe v
  insert       :: k -> v -> GMap k v -> GMap k v

instance GMapKey () where
  data GMap () v       = GMU (Maybe v)
  empty                = GMU Nothing
  lookup () (GMU mv)   = mv
  insert () v (GMU _)  = GMU $ Just v

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v           = GME (GMap a v) (GMap b v)
  empty                              = GME empty empty
  lookup (Left  a)  (GME gm1  _gm2)  = lookup a gm1
  lookup (Right b)  (GME _gm1 gm2 )  = lookup b gm2
  insert (Left  a)  v (GME gm1 gm2)  = GME (insert a v gm1) gm2
  insert (Right a)  v (GME gm1 gm2)  = GME gm1 (insert a v gm2)

instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v        = GMP (GMap a (GMap b v))
  empty		            = GMP empty
  lookup (a, b) (GMP gm)    = lookupGMP a b gm          -- TODO
  insert (a, b) v (GMP gm)  = GMP (insertGMP a b v gm)  -- TODO

{- Part 3 (a): Fully expand the type family application 
|GMap (Either () (Bit, a)) v|. You may ignore the constructors |GMU|,
|GME| and |GMP| as I did in the comment after type signature for |t0|.

----

  GMap (Either () (Bit, a)) v   
~= {- instance for Either -}
  (GMap () v, GMap (Bit, a) v)
~= {- instances for () and (,) -}
  (Maybe v, GMap Bit (GMap a v))
~= {- expand type synonym Bit -}
  (Maybe v, GMap (Either () ()) (GMap a v))
~= {- instance for Either -}
  (Maybe v, (GMap () (GMap a v), (GMap () (GMap a v))))
~= {- instance for (), twice -}
  (Maybe v, (Maybe (GMap a v), Maybe (GMap a v)))

-}

-- ----------------
  
{- Task 3 (b): Give the type signatures for and implement |lookupGMP|
and |insertGMP|. -}

lookupGMP :: (GMapKey a, GMapKey b) => a -> b -> GMap a (GMap b v) -> Maybe v
lookupGMP a b = lookup a >=> lookup b

insertGMP :: (GMapKey a, GMapKey b) => a -> b -> v -> 
             GMap a (GMap b v) -> GMap a (GMap b v)
insertGMP a b v gm = insert a (insert b v innermap) gm
  where innermap = lookupDef empty a gm

lookupDef :: GMapKey k => v -> k -> GMap k v -> v
lookupDef def k = maybe def id . lookup k


-- ----------------------------------------------------------------

{- Task 3 (c): Give type signatures for and implement |fmapGMU|,
|fmapGME| and |fmapGMP|. -}

-- Code from the exam question:
instance Functor (GMap ()) where
  fmap = fmapGMU
  
instance (Functor (GMap a), Functor (GMap b)) => 
         Functor (GMap (Either a b)) where
  fmap = fmapGME
  
instance (Functor (GMap a), Functor (GMap b)) => 
         Functor (GMap (a, b)) where
  fmap = fmapGMP
  
-- Answer:

fmapGMU :: (a->b) -> GMap () a -> GMap () b
fmapGMU f (GMU ma) = GMU $ fmap f ma
-- the inner fmap is for Maybe

fmapGME :: (Functor (GMap c), Functor (GMap d)) => 
           (a->b) -> GMap (Either c d) a -> GMap (Either c d) b
fmapGME f (GME gma gmb) = GME (fmap f gma) (fmap f gmb)           

fmapGMP :: (Functor (GMap c), Functor (GMap d)) => 
           (a->b) -> GMap (c, d) a -> GMap (c, d) b
fmapGMP f (GMP gmab) = GMP $ fmap (fmap f) gmab

