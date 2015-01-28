{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ParserFromStdLib where

import Control.Monad
import qualified Control.Monad.State as CMS

newtype P s a = P {unP :: CMS.StateT [s] [] a}
  deriving (Monad, MonadPlus, CMS.MonadState [s])
           
type ParseResult s a =  [(a, [s])]
parse       :: P s a -> [s] -> ParseResult s a
parse = CMS.runStateT . unP 

symbol :: P s s
symbol = do  (s:rest) <- CMS.get  -- ^ Note that this will use fail in case of empty input
             CMS.put rest
             return s

{- When we are into using the standard monad combinators, we can just
as well admit that pfail and (+++) are the standard methods mzero and
mplus from the MonadPlus class.
-}

pfail       :: P s a
pfail = mzero
--pfail = fail "This string is ignored in the list monad"

(+++)       :: P s a -> P s a -> P s a
(+++) = mplus


----------------

{-
symbol :: P s s
symbol = do  inp <- CMS.get
             case inp of
               []      -> pfail
               (s:ss)  -> do  CMS.put ss
                              return s
-}

