{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NewtypesAndDeriving where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

type S = StateT Int Identity
type Q = StateT Int (ReaderT Char IO)


-- instances for Monad S, Functor S, MonadState Int S, 
-- instances for Monad Q, Functor Q, MonadState Int Q, MonadReader Char Q, 

newtype NS a = NS {unNS :: StateT Int Identity a}
  deriving (Monad, Functor, MonadState Int)

newtype NQ a = NQ {unNQ :: StateT Int (ReaderT Char IO) a}

-- no instances at all

newtype Age = Age Int
-- restrict instances

newtype Wrap a = Wrap {unWrap :: a}
--   deriving (
