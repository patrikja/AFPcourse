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

-- Some OK variations:
liftPc'   :: (a -> b -> c) -> InfoP a ->       b -> InfoP c
liftPc' op ia b   = \ f  p  s  t -> ia f p s t `op` b
pathP'    :: InfoP FilePath
pathP'            = liftPath id
constP'    :: a -> InfoP a
constP' = const.const.const.const

-- Some examples of errors
-- redefine InfoP as a deep embedding (making all the operations contructors in a datatype)
-- Use monadic do-notation (without providing a Monad instance)
-- liftPc f ip = liftP2 f ip $ constP
-- (==??) iA a fp p i utc = a == $ iA fp p i utc
-- liftP2 f (Const a) (Const b) = Const (f a b)
-- ...

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


-- Variants
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' xs (body :: a -> m b) = foldr step base xs
   where base :: m [b]
         base = return []
         step :: a -> m [b] -> m [b]
         step x = liftM2 (:) (body x)
         -- or    liftM2 (:) . body
-- But only if you also implement liftM2
liftM2, liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2  op ma mb = ma >>= \a -> liftM (op a) mb
liftM2' op ma mb = ma >>= \a -> mb >>= \b -> return (op a b)

shared :: Monad m => (a -> res -> [b] -> [b]) -> (a -> m res) -> [a] -> m [b]
shared f fm =foldr (\x m -> fm x >>= \res -> liftM (f x res) m) (return [])
filterM'' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM''    = shared (\x b -> if b then (x:) else id)
forM''    :: Monad m => [a] -> (a -> m b) -> m [b]
forM'' xs fm = shared (const (:)) fm xs

-- ================================================================
-- Some errors on the b) part
--   type1 = a -> [m a] -> [m a]
--   type2 = a -> [m b] -> [m b]
--   filterM f xs = liftM $ foldr (\a1 a2 -> ...) [] xs

-- "Almost right"s:
-- (in filterM): step x = mp x >>= \b -> if b then liftM (x:) else id
-- (in forM):    step x = body x >>= \b -> liftM (b:)

-- Working but unnecessarily complex
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _f []      = return []
filterM'  f (l:ls)  = mergeLM (testM f l) (filterM f ls)

testM :: (Monad m) => (a -> m Bool) -> a -> m [a]
testM f a = (f a) >>= \b -> if b then return [a] else return []

mergeLM :: (Monad m) => m [a] -> m [a] -> m [a]
mergeLM mas mbs = mas >>= \as -> mbs >>= \bs -> return (as ++ bs)


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
