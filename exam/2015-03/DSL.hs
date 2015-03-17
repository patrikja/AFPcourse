{-# LANGUAGE ScopedTypeVariables #-}
module DSL where
import Control.Monad(liftM)
----------------------------------------------------------------
-- Problem 1a:
constP a         = \_f _p _s _t -> a
liftPc op ia b   = liftP2 op ia (constP b)
liftP2 op ia ib  = \ f  p  s  t -> ia f p s t `op` ib f p s t
liftPath fp      = \ f _p _s _t -> fp f
(&&?)            = liftP2 (&&)
(||?)            = liftP2 (||)
pathP            = \ f _p _s _t -> f
sizeP            = \_f _p  s _t -> s
(==?)            = liftPc (==)
(>?)             = liftPc (>)

-- Some variations:
liftPc'   :: (a -> b -> c) -> InfoP a ->       b -> InfoP c
liftPc' op ia b   = \ f  p  s  t -> ia f p s t `op` b

----------------------------------------------------------------
-- Problem 1b: Implement filterM and forM using just return, (>>=), liftM and foldr

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM (mp :: a -> m Bool) = foldr step base
  where base :: Monad m => m [a]
        base = return []
        step :: Monad m => a -> m [a] -> m [a]
        step x mxs = mp x >>= \b -> liftM (if b then (x:) else id) mxs

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs (body :: a -> m b) = foldr step base xs
   where base :: m [b]
         base = return []
         step :: a -> m [b] -> m [b]
         step x mxs = body x >>= \b -> liftM (b:) mxs


-- ================================================================

-- Exam question text:

-- newtype FilePath     = FilePath () -- Imported from the Prelude
newtype Permissions  = Permissions () -- Not part of exam question
newtype UTCTime      = UTCTime ()     -- Not part of exam question
  
type InfoP a = FilePath -> Permissions -> Integer -> UTCTime -> a
find = error "Not part of exam question"
myTest :: InfoP Bool
myTest = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 100000)
  where takeExtension :: FilePath -> String
        takeExtension = error "Not part of exam question"
test :: IO [FilePath]
test = find "/home/patrikj/src" myTest


constP    :: a -> InfoP a
liftPc    :: (a -> b -> c) -> InfoP a ->       b -> InfoP c
liftP2    :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftPath  :: (FilePath -> a) -> InfoP a
(&&?)     :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?)     :: InfoP Bool -> InfoP Bool -> InfoP Bool
pathP     :: InfoP FilePath
sizeP     :: InfoP Integer
(==?)     :: (Eq a)   => InfoP a -> a -> InfoP Bool
(>?)      :: (Ord a)  => InfoP a -> a -> InfoP Bool

type Predicate = InfoP Bool
find :: FilePath -> Predicate -> IO [FilePath]  -- run function
         
