module Compiler.Generators where
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..), Op1(..), Op2(..))

import Test.QuickCheck 
import Control.Monad       (liftM, liftM2, liftM3)

arbName :: Gen Name
arbName = sized $ \n -> 
  elements [ [c] | c <- take (n `div` 3 + 1) ['a'..'z'] ]

shrinkExpr :: Expr -> [Expr]
shrinkExpr  (Uno o e)        = e : 
           [ Uno o e' | e' <- shrinkExpr e ]
shrinkExpr  (Duo o e1 e2)    = e1 : e2 :
           [ Duo o e1' e2 | e1' <- shrinkExpr e1 ] ++
           [ Duo o e1 e2' | e2' <- shrinkExpr e2 ]
shrinkExpr  (Val v) = [ Val v' | v' <- shrinkValue v ]
shrinkExpr  (Var x) = [ Val (Num 0)
                      , Val (Bol False)
                      , Val (Bol True) ]

-- | arbExpr includes "badly typed" expressions
arbExpr :: Gen Expr
arbExpr = sized arbSizedExpr

arbSizedExpr :: Int -> Gen Expr
arbSizedExpr n = frequency $
    [ (4, liftM Var arbName)
    , (1, liftM Val arbValue)
    ] ++
    concat
    [ [ (2, liftM2 Uno arbUno arbExpr')
      , (4, liftM3 Duo arbDuo arbExpr2 arbExpr2)
      ]
    | n > 0  -- useful list comprehension trick
    ]
  where
    arbExpr' = arbSizedExpr (n-1)
    arbExpr2 = arbSizedExpr (n `div` 2)

arbUno = arbEnum
arbDuo = arbEnum

arbCommand :: Gen Command
arbCommand = sized arbSizedCom

arbSizedCom :: Int -> Gen Command
arbSizedCom n = frequency $
    [ (1, return Skip)
    , (6, liftM2 (:=) arbName arbExpr)
    , (3, liftM  Print arbExpr)
    ] ++
    concat
    [ [ (4, liftM2 (:->) arbCommand2 arbCommand2)
      , (4, liftM3 If    arbExpr arbCommand2 arbCommand2) 
      , (4, liftM2 While arbExpr arbCommand')
      ]
    | n > 0  -- useful list comprehension trick
    ]
  where
    arbCommand' = arbSizedCom (n-1)
    arbCommand2 = arbSizedCom (n `div` 2)

shrinkCommand :: Command -> [Command]
shrinkCommand c = case c of
  (x := e)    -> Skip : [ x := e' | e' <- shrinkExpr e ]
  (c1 :-> c2) -> c1 : c2 :
                 [ c1' :-> c2   | c1' <- shrinkCommand c1 ] ++
                 [ c1  :-> c2'  | c2' <- shrinkCommand c2 ]
  (If e c1 c2)-> c1 : c2 :
                 [ If e' c1 c2  | e'  <- shrinkExpr e  ] ++
                 [ If e c1' c2  | c1' <- shrinkCommand c1 ] ++
                 [ If e c1 c2'  | c2' <- shrinkCommand c2 ]
  (While e c) -> c :
                 [ While e' c   | e' <- shrinkExpr e ] ++
                 [ While e c'   | c' <- shrinkCommand c ]
  (Print e)   -> Skip : [ Print e' | e' <- shrinkExpr e ]
  _           -> []

arbEnum :: (Bounded a, Enum a) => Gen a
arbEnum = elements [ minBound .. maxBound ]

arbValue :: Gen Value
arbValue = frequency $
  [ (4, Num `liftM` arbitrary)
  , (4, Bol `liftM` arbitrary)
  , (1, return Wrong)
  ]

shrinkValue :: Value -> [Value]
shrinkValue (Num n) = map Num (shrink n)
shrinkValue (Bol b) = Num 0 : map Bol (shrink b)
shrinkValue Wrong   = [ Num 0 ]

{-
elements' s [] = error $ "elements used with empty list in " ++ s
elements' _ xs = elements xs
-}


