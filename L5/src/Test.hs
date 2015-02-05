{-# LANGUAGE FlexibleContexts #-}
module Test where

-- import qualified Control.Applicative    as CA (Applicative(..))
import qualified Control.Monad          as CM
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS
import qualified Control.Monad.Error    as CME -- new



test1 :: ( CMS.MonadState  s m
         , CMR.MonadReader t m) =>
         (t -> s -> s) -> (t -> b) -> m b
test1 op f = do e <- CMR.ask
                s <- CMS.get
                CMS.put (op e s)
                return (f e)

{-
-- Monad transformer intuition (to be drawn on the board)

Somewhat simplified a type class is a set of monomorphic
instances. For Monad that set (which we can call M) contains Identity,
Maybe, [], IO, but also infinitely many other (like Reader env for
every type env). A monad transformer is a (type-level) function from M
to M.

The MonadReader class can be thought of as a subset (call it R) of M.
This subset contains only those monads for which there is an
implementation of the methods ask and local. Similarly we can consider
the subset S of M for monads supporting get and put etc.

If we write a piece of monadic code using (say) ask and put we need to
find an element (a monad) in the intersection of the subsets R and S
to be able to run the code (call the intersection RS - a subset of
both S and R). Such a monad can be built "by hand" by providing a
suitable datatype and implementing return, bind, ask, local, put and
get. Or it can be built by combining monad transformers.

The state monad transformer takes any monad m into another monad
StateT s m which is in the subset S. Similarly ReaderT env m takes m
into the subset R. And we can combine them to construct an element of
RS. In fact we can do this in two ways
  StateT s (ReaderT env m)   ~=   s -> r -> (a, s)
or
  ReaderT env (StateT s m)   ~=   r -> s -> (a, s)
Both are monads supporting MonadReader and MonadState and they are in
practice "the same" (they are isomorphic).

-}
