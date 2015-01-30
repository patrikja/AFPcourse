-- | Copy from hackage - stripped down to the bare essentials.
module Control.Monad.Identity (
    Identity(..),
    module Control.Monad,
   ) where

import Control.Monad

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

