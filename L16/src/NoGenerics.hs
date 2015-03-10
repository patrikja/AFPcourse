{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleContexts #-}
module NoGenerics where
import Control.Applicative  (Applicative, pure, (<*>), (<$>))
import qualified Control.Monad.Reader 
              as CMR        (MonadReader, runReader, ask, local)
import Data.Traversable     (Traversable, traverse)
import Data.Foldable        (Foldable, foldMap)
import Data.Monoid          (mempty, mappend)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

-- We split our datatype into the top-level structure (Expr')
-- and the recursive knot-tying (Expr)
type Name = String
data Expr' e = Var Name | App e e | Lam Name e
newtype Expr = Expr { unExpr :: Expr' Expr }

-- Expr' has all kinds of nice properties it is in 
--   Functor, Foldable, Traversable
-- For freeVars we only need Foldable (and Monoid (Set a)).

-- foldMap :: (Foldable t, Monoid m) => 
--            (a -> m) -> t a -> m
instance Foldable Expr' where
  foldMap _f (Var _)     = mempty
  foldMap  f (App e1 e2) = f e1  `mappend`  f e2
  foldMap  f (Lam _ e)   = f e

-- Once we get the instances out of the way we can define
-- @freeVars@ quite elegantly.

freeVars :: Expr -> Set Name
freeVars (Expr e) = case e of
  Var x   -> Set.singleton x
  Lam x b -> Set.delete x (freeVars b)
  _       -> foldMap freeVars e

-- Another more interesting example: alphaRename

instance Functor Expr' where
  fmap _f (Var x)     = Var x
  fmap  f (App e1 e2) = App   (f e1) (f e2)
  fmap  f (Lam x e)   = Lam x (f e)

-- traverse :: (Traversable t, Applicative f) => 
--             (a -> f b) -> t a -> f (t b)
instance Traversable Expr' where
  traverse _f (Var x)     = pure (Var x)
  traverse  f (App e1 e2) = App    <$>  f e1  <*>  f e2
  traverse  f (Lam x e)   = Lam x  <$>  f e

type Supply = [Name]
nameS :: Supply
nameS = [ s ++ [c] | s <- "":nameS, c <- ['a'..'z'] ]
type Renaming = Map Name Name
type Env = (Supply, Renaming)

alphaRename :: Expr -> Expr
alphaRename e = CMR.runReader (alpha e) (nameS, Map.empty)

-- alpha :: CMR.MonadReader Env m => Expr -> m Expr    
alpha :: (CMR.MonadReader Env m, Applicative m) =>
         Expr -> m Expr
alpha (Expr e) = Expr <$> case e of
  Var x   -> Var <$> rename x
  Lam x b -> fresh x $ \y -> Lam y <$> alpha b
  _       -> traverse alpha e

rename :: (CMR.MonadReader (s, Map n n) m, Ord n) => n -> m n
rename x = do
  (_supply, ren) <- CMR.ask
  return $ maybe x id $ Map.lookup x ren

fresh :: (CMR.MonadReader ([t], Map k t) m, Ord k) => 
         k -> (t -> m b) -> m b
fresh x f = do
  (y:names, ren) <- CMR.ask
  f y   `inEnv`  (names, Map.insert x y ren) 

inEnv :: CMR.MonadReader b m => m a -> b -> m a
mx `inEnv` env = CMR.local (const env) mx

{-

-- This was earlier missing from the Haskell libraries. Not
-- needed with ghc 6.12 or later

instance Applicative (Reader e) where
  pure  = return
  (<*>) = ap
-}

-- Examples

lam :: Name -> Expr -> Expr
lam x e   = Expr (Lam x e)
app :: Expr -> Expr -> Expr
app e1 e2 = Expr (App e1 e2)
var :: Name -> Expr
var x     = Expr (Var x)

ex1, ex2, ex3 :: Expr
ex1 = lam "x" $ var "x"
ex2 = lam "unused" $ var "C"
ex3 = lam "x" $ lam "y" $ lam "z" $ 
      app (var "x") (var "z") `app` 
      app (var "y") (var "z")

instance Show Expr where
  showsPrec p (Expr e) = showsPrec p e

instance Show e => Show (Expr' e) where
  showsPrec _ (Var x) = showString x
  showsPrec p (App e1 e2) = showParen (p>1) $
    showsPrec 1 e1 . showString " " . showsPrec 2 e2
  showsPrec p (Lam x e) = showParen (p>0) $
    showString ("\\" ++ x ++ "-> ") . shows e

test :: Expr -> IO ()
test e = do printf "e=%-25s;  free(e)=%-5s;  α(e)=%-25s\n" 
              (show e) 
              (show $ Set.toList $ freeVars e) 
              (show $ alphaRename e)
          
main :: IO ()
main = do test ex1
          test ex2
          test ex3
