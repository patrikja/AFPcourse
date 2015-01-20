
module Parser where

import Control.Applicative hiding ((<|>))
import Data.Char
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec

data Language e =
  Lang { lLit    :: Integer -> e
       , lPlus   :: e -> e -> e
       , lLet    :: String -> e -> e -> e
       , lVar    :: String -> e
       , lNewref :: e -> e
       , lDeref  :: e -> e
       , lAssign :: e -> e -> e
       , lCatch  :: e -> e -> e
       }

tok :: TokenParser st
tok = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = satisfy (\c -> isAlpha c || c == '_')
  , identLetter     = satisfy (\c -> isAlphaNum c || c == '_')
  , opStart         = satisfy (`elem` "+:=!;")
  , opLetter        = satisfy (`elem` "=")
  , reservedNames   = ["let", "new", "try", "catch"]
  , reservedOpNames = ["+", ":=", "=", "!", ";"]
  , caseSensitive   = True
  }

parseExpr :: Language e -> String -> Either ParseError e
parseExpr lang = parse exprP ""
  where

    exprP = do
      e <- expr0
      eof
      return e

    expr0 = choice
      [ do reserved tok "let"
           x <- identifier tok
           reservedOp tok "="
           e1 <- expr2
           reservedOp tok ";"
           e2 <- expr0
           return $ lLet lang x e1 e2
      , do reserved tok "try"
           e1 <- expr0
           reserved tok "catch"
           e2 <- expr0
           return $ lCatch lang e1 e2
      , expr1
      ]

    expr1 = chainr1 expr2 (reservedOp tok ";" >> return (lLet lang "_"))
    expr2 = chainr1 expr3 (reservedOp tok ":=" >> return (lAssign lang))
    expr3 = chainl1 expr4 plusP
    expr4 = choice
      [ atomP
      , do reservedOp tok "!"
           e <- expr4
           return (lDeref lang e)
      , do reserved tok "new"
           e <- expr4
           return (lNewref lang e)
      ]

    atomP = choice
      [ lLit lang <$> integer tok
      , lVar lang <$> identifier tok
      , parens tok expr0
      ]

    plusP = reservedOp tok "+" >> return (lPlus lang)

