{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Applicative
import Foreign (Storable(..), Ptr, alloca)
import Foreign.C (CInt(..), CString, withCString, peekCString)
import System.IO.Unsafe(unsafePerformIO)
import Text.Printf (printf)

-- For simple types like integers and strings, the Foreign.C
-- library contains the C versions of the types and functions
-- for converting back and forth.

-- We can import foreign functions as pure functions, but we are
-- responsible for making sure that they don't have unpleasant
-- side effects.
foreign import ccall "inc" c_inc :: CInt -> CInt

inc :: Int -> Int
inc n = fromIntegral $ c_inc (fromIntegral n)

-- If a function isn't pure, we import it with IO type.
foreign import ccall "encrypt" c_encrypt :: CString -> IO ()

-- Since the side effects are local (creating a string and doing
-- things to it) we can safely pretend that our wrapper is side
-- effect free.
encrypt :: String -> String
encrypt s = unsafePerformIO $ withCString s $ \cs -> do
  c_encrypt cs
  peekCString cs

-- For more complex foreign types we give an instance of the
-- Storable class to explain how to read and write them from
-- memory. In this example the type on the C side is
--   struct { int x, y; }

-- First declare the corresponding Haskell type.
data Point = Pt Int Int
  deriving Show

-- Import functions to read and write the components.
foreign import ccall "init_pt"   c_init_pt :: Ptr Point -> 
                                              CInt -> CInt -> IO ()
foreign import ccall "get_x"     c_get_x   :: Ptr Point -> IO CInt
foreign import ccall "get_y"     c_get_y   :: Ptr Point -> IO CInt
foreign import ccall "sqdist"    c_sqdist  :: Ptr Point -> CInt
foreign import ccall "sizeof_pt" c_sizeof_pt :: CInt  
 -- still needs to be a function on the C side

-- Give the storable instance. For portability we should use
-- foreign functions to manipulate the structure.
instance Storable Point where
  sizeOf _    = fromIntegral c_sizeof_pt
  alignment _ = 4
  peek p = do
    x <- c_get_x p
    y <- c_get_y p
    return $ Pt (fromIntegral x) (fromIntegral y)
  poke p (Pt x y) = c_init_pt p (fromIntegral x) (fromIntegral y)

createPt :: Int -> Int -> Point
createPt x y = unsafePerformIO $ alloca $ \p -> do
  c_init_pt p (fromIntegral x) (fromIntegral y)
  peek p

sqDist :: Point -> Int
sqDist pt = unsafePerformIO $ alloca $ \p -> do
  poke p pt
  return $ fromIntegral $ c_sqdist p

----------------
test1 :: Int
test1 = inc 1737
test2 :: (String, String)
test2 = (encrypted, encrypt encrypted)
  where encrypted = encrypt "Patrik"
test3 :: Int
test3 = sqDist (Pt 3 4)

main :: IO ()
main = do print test1
          uncurry (printf "(%s,%s)\n") test2
          print test3
