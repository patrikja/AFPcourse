module Spec where
import Types

import qualified Prelude
import Prelude hiding ((>>=),return)
import Data.Monoid

-- a) 
--   fmap return  :: Monad2 m => m a          -> m (m a)
--   join         :: Monad2 m => m (m (m a))  -> m (m a)

units :: Monad2 m => [m a -> m a]
units = [ join . return
        , id
        , join . (fmap return :: Monad2 m => m a -> m (m a))
        ]

assoc :: Monad2 m => [m (m (m a)) -> m a]
assoc = [ join . fmap join 
        , join . (join :: Monad2 m => m (m (m a)) -> m (m a))
        ]


-- b)

z :: Monoid w => w
z = mempty

instance Monoid w => Monad2 ((,) w) where
    return a           = (z        , a)
    join (w, (w', a))  = (w <> w'  , a)

assocProof :: Monoid w => (w, (w, (w, a))) -> [(w, a)]
assocProof (w1, (w2, (w3, a))) = 
    [ (join . fmap join) (w1, (w2, (w3, a)))
    , -- Def. |(.)|
      join   (fmap join  (w1, (w2, (w3, a))))
    , -- Def. |fmap|
      join (w1,    join       (w2, (w3, a)))
    , -- Def. |join|
      join (w1,               (w2 <> w3, a))
    , -- Def. |join|
      (w1 <> (w2 <> w3), a)
    , -- Monoid law (assoc. |(<>)|)
      ((w1 <> w2) <> w3, a)
    , -- Def. |join|
      join (w1 <> w2,              (w3, a))
    , -- Def. |join|
      join (join    (w1, (w2,      (w3, a))))
    , -- Def. |(.)|
      (join . join) (w1, (w2,      (w3, a)))
    ]

unitProof :: Monoid w => (w, a) -> [(w, a)]
unitProof (w, a) = 
    [ (join . return) (w, a)
    , -- Def. |(.)|
      join (return (w, a))
    , -- Def. |return|
      join (z, (w, a))
    , -- Def. |join|
      (z <> w, a)
    , -- Monoid law
      (w, a)
    , -- Def. |id|. Left part of the law done.
      id (w, a)
    , -- Def. |id|. Right part proof starting.
      (w, a)
    , -- Monoid law
      (w <> z, a)
    , -- Def. |join|
      join (w, (z, a))
    , -- Def. |return|
      join (w, return a)
    , -- Def. |fmap|
      join (fmap return (w, a))
    , -- Def. |(.)|
      (join . fmap return) (w, a)
    ]
