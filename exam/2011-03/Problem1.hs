module Problem1 where
import qualified Prelude as P
import Prelude(Functor, (.), id, (++), Eq, (==), Bool(..), 
               Show, showsPrec, shows)
import Data.Monoid(Monoid, mempty, mappend)
import Test.QuickCheck

-- Problem 1a)
newtype DList a = DL { unDL :: [a] -> [a] }

append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL (xs . ys)

cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)

empty :: DList a
empty = DL id

foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f z xs = P.foldr f z (toList xs)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) empty

toList :: DList a -> [a]
toList (DL xs) = xs []
-- Alternatively: toList xs = unDL xs []

instance Functor DList where
  fmap = map

instance Monoid (DList a) where
  mempty = empty
  mappend = append

instance Eq a => Eq (DList a) where
  xs == ys   =   toList xs == toList ys

----------------
-- Problem 1b)

{-
Functor laws:
       fmap id == id
  fmap (f . g) == fmap f . fmap g

Monoid laws:  
  mappend mempty m == m
  mappend m mempty == m
  mappend (mappend m_1 m_2) m_3 == mappend m_1 (mappend m_2 m_3)

-}

-- Polymorphic properties:
prop_fmap_id :: P.Eq a => DList a -> Bool
prop_fmap_id xs  =  map id xs == id xs

prop_fmap_comp :: Eq c => (b->c) -> (a->b) -> DList a -> Bool
prop_fmap_comp f g =  map (f . g) === map f . map g

infix 4 ===
(===) :: (Eq a) => (t -> a) -> (t -> a) -> t -> Bool
f === g = \x -> f x == g x

prop_monoid_1 :: (Monoid a, Eq a) => a -> Bool
prop_monoid_1 m =  mappend mempty m == m

prop_monoid_2 :: (Monoid a, Eq a) => a -> Bool
prop_monoid_2 m =  mappend m mempty == m

prop_monoid_3 :: (Monoid a, Eq a) => a -> a -> a -> Bool
prop_monoid_3 m1 m2 m3 =  mappend (mappend m1 m2) m3 == mappend m1 (mappend m2 m3)

-- Monomorphic versions for quickCheck
type B = Bool -- could be some other non-trivial type (not ())

test_id   = quickCheck (prop_fmap_id   :: DList Bool -> Bool)
test_comp = quickCheck (prop_fmap_comp :: (B->B)->(B->B) -> DList B -> Bool)
test_mon1 = quickCheck (prop_monoid_1  :: DList B -> Bool)
test_mon2 = quickCheck (prop_monoid_2  :: DList B -> Bool)
test_mon3 = quickCheck (prop_monoid_3  :: DList B -> DList B -> DList B -> Bool)


instance Show a => Show (DList a) where -- not required on the exam
  showsPrec p xs = showsPrec p (toList xs)
    
instance (P.Bounded a, P.Enum a, Show a, Show b) => Show (a->b) where
  showsPrec p f = showsPrec p (P.map (\x-> (x,f x)) [P.minBound .. P.maxBound])

instance Arbitrary a => Arbitrary (DList a) where
  arbitrary = P.fmap fromList arbitrary

main = do 
  test_id   
  test_comp 
  test_mon1 
  test_mon2
  test_mon3 

----------------------------------------------------------------
-- Problem 1 extras: attempting to "prove" some properties by equality
-- reasoning. Not part of the exam question and unfinished.

functor_law_1 =        
  [ map id
  , foldr (cons . id) empty
  , foldr cons empty
  , -- foldr_cons_empty_lemma
    id
  ]

-- foldr cons empty xs == xs
foldr_cons_empty_lemma_e =
  [ foldr cons empty empty
  , P.foldr cons empty (toList empty)
  , P.foldr cons empty (toList (DL id))
  , P.foldr cons empty (id [])
  , P.foldr cons empty []
  , empty
  ]
foldr_cons_empty_lemma_c x xs =
  [ foldr cons empty (cons x xs)
  , foldr cons empty (DL ((x:) . unDL xs))
  , P.foldr cons empty (toList (DL ((x:) . unDL xs)))
  , P.foldr cons empty (((x:) . unDL xs) [])
  , P.foldr cons empty (x : unDL xs [])
  , cons x (P.foldr cons empty (unDL xs []))
  , cons x (P.foldr cons empty (toList xs))
  -- lemma P.foldr cons empty (toList xs)
  ]

-- P.foldr cons empty (toList xs) == xs
lemma2 xs = 
  [ P.foldr cons empty (toList xs)
  , xs
  ]  

