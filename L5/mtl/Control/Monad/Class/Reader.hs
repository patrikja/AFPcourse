module Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a
