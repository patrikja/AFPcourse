{-# LANGUAGE StandaloneDeriving #-} 
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances  #-}
module Main where
import Types

----------------------------------------------------------------

-- Some example code from the exam question
type Bit = Either () ()
o  = Left  (); i  = Right ()

type Four = (Bit, Bit)
oo = (o, o); oi = (o, i); io = (i, o); ii = (i, i)

t0, t1, t2 :: GMap Four Int
t0 = empty
t1 = insert oi 17 t0
t2 = insert io 38 t1

main = do print t0
          print t1
          print t2
          
-- Small test
t3 = fmap (1+) t2

----------------------------------------------------------------
{-
instance Show v => Show (GMap Four v) where
  show gm = concatMap showElem [oo, oi, io, ii]
    where showElem x = maybe "_" show (lookup x gm)
-}

deriving instance (Show v) => Show (GMap () v)
deriving instance (Show (GMap b v), Show (GMap a v)) => Show (GMap (Either a b) v)
deriving instance (Show (GMap a (GMap b v))) => Show (GMap (a, b) v)

-- Not part of the exam question

{- 
lemma:  
  
  GMap Bit v
~= 
  GMap (Either () ()) v
~= 
  (GMap () v, GMap () v)
~= 
  (Maybe v, Maybe v)


  GMap Four v 
~= 
  GMap (Bit, Bit) v
~= 
  GMap Bit (GMap Bit v)
~= lemma for the outer GMap Bit
  (Maybe (GMap Bit v), Maybe (GMap Bit v))
~= lemma for the inner GMap Bits
  (Maybe (Maybe v, Maybe v), Maybe (Maybe v, Maybe v))

-}
