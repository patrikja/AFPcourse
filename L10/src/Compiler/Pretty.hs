module Compiler.Pretty where

import Text.PrettyPrint

import Compiler.Value
import Compiler.Syntax

-- Comment out the Command instance to debug the pretty printer
instance Show Command where
  show = show . pretty

class Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Command where
    pretty Skip     = text "skip"
    pretty (x := e) = sep   [ text x <+> text ":="
                            , nest 2 $ pretty e
                            ]
    pretty (Print e) = sep  [ text "print"
                            , nest 2 $ pretty e
                            ]
    pretty (c1 :-> c2) = sep [ pretty c1 <+> text ";", pretty c2] 
                         -- vcat [ pretty c1 <+> text ";", pretty c2 ]
    pretty (If e c1 c2) = vcat  [ sep   [ text "if"
                                        , nest 4 $ pretty e
                                        , nest 4 $ text "then"
                                        ]
                                , nest 2 $ pretty c1
                                , text "else"
                                , nest 2 $ pretty c2
                                , text "fi"
                                ]
    pretty (While e c)  = vcat  [ sep   [ text "while"
                                        , nest 4 $ pretty e
                                        , nest 4 $ text "do"
                                        ]
                                , nest 2 $ pretty c
                                , text "od"  
                                ]

instance Pretty Value where
    prettyPrec p (Num n)
        | n >= 0    = text $ show n
        | otherwise = mparens (p>0) $ text $ show n
    prettyPrec p (Bol True)     = text "T"
    prettyPrec p (Bol False)    = text "F"
    prettyPrec _ Wrong          = text "Wrong"

instance Pretty Expr where
    prettyPrec p (Var x)        = text x
    prettyPrec p (Val v)        = prettyPrec p v
    prettyPrec p (Uno Minus e)  = mparens (p>0) $ text "-" <> prettyPrec 10 e
    prettyPrec p (Uno Not e)    = mparens (p>9) $ text "!" <> prettyPrec 10 e
    prettyPrec p (Duo op e1 e2) = mparens (p>prec op) $
                                    sep [ prettyPrec (precL op) e1
                                        , nest 2 $ sep  [ pretty op
                                                        , nest 2 $ prettyPrec (precR op) e2
                                                        ]
                                        ]

instance Pretty Op2 where
    pretty op = text $ case op of
                    And     -> "&"
                    Or      -> "|"
                    Mul     -> "*"
                    Add     -> "+"
                    Sub     -> "-"
                    Div     -> "/"
                    Mod     -> "%"
                    Less    -> "<"
                    LessEq  -> "<="
                    Eq      -> "=="

prec op     = head [ p | (op', _, p) <- ops, op == op' ]
assoc op    = head [ f | (op', f, _) <- ops, op == op' ]
precL op = case assoc op of
                L   -> prec op
                _   -> prec op + 1
precR op = case assoc op of
                R   -> prec op
                _   -> prec op + 1

data Assoc = L | R | N

ops =   
  [ (And, L, 2)
  , (Or,  L, 1)
  , (Mul, L, 7)
  , (Add, L, 6)
  , (Sub, L, 6)
  , (Div, L, 7)
  , (Mod, L, 7)
  , (Less,   N, 3)
  , (LessEq, N, 3)
  , (Eq,     N, 3)
  ]

mparens True  d  = cat [ text "(" <> d, text ")" ]
mparens False d  = d
