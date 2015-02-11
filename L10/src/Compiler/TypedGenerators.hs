module Compiler.TypedGenerators where
import Compiler.Syntax      (Command(..), Expr(..), Name)
import Compiler.Value       (Value(..), Op1(..), Op2(..))
import Compiler.Generators (arbName)

import Test.QuickCheck 
import Control.Monad       (liftM, liftM2)
import Data.Map            (Map)
import qualified Data.Map as Map
----------------------------------------------------------------
-- Generating well-typed programs (optional)

data Type = TInt | TBool | Undef
  deriving (Eq, Show)

instance Arbitrary Type where
  arbitrary = elements [TInt, TBool]

type Env = Map Name Type

-- Not used
-- type G = StateT Env Gen  -- ^ QuickCheck generator + type env.

unionEnv :: Env -> Env -> Env
unionEnv env1 env2 = Map.unionWith unionType env1 env2
  where unionType Undef _  =  Undef
        unionType _ Undef  =  Undef
        unionType s    _t  =  s

genValue :: Type -> Gen Value
genValue TInt  = liftM Num arbitrary
genValue TBool = liftM Bol arbitrary
genValue Undef = return Wrong

genExpr :: Env -> Type -> Gen Expr
genExpr env t = sized $ \n -> genSizedExpr env n t

genSizedExpr :: Env -> Int -> Type -> Gen Expr
genSizedExpr env n t = do
    let okVars = [ x | (x, t') <- Map.toList env, t == t' ]
    frequency $
      [ (1, liftM Var $ elements okVars) 
      | not $ null okVars ] ++
      [ (1, liftM Val $ genValue t) ] ++
      concat
      [ [ (2, uno)
        , (4, duo)
        ]
      | n > 0
      ]
  where
      gen' = genSizedExpr env (n-1)
      gen2 = genSizedExpr env (n `div` 2)

      uno = do
        let okOps = [ args | (args, resT) <- unoOps
                           , resT == t
                    ]
        (op, argT) <- elements okOps
        liftM (Uno op) (gen' argT)

      duo = do
        let okOps = [ args
                    | (args, resT) <- duoOps
                    , resT == t
                    ]
        (op, argT1, argT2) <- elements okOps
        liftM2 (Duo op) (gen2 argT1) (gen2 argT2)

unoOps :: [ ((Op1,   Type),  Type)]
unoOps =  [ ((Not,   TBool), TBool)
          , ((Minus, TInt),  TInt)
          ]

duoOps :: [ ((Op2,    Type,  Type),  Type) ]
duoOps =  [ ((And,    TBool, TBool), TBool)
          , ((Or,     TBool, TBool), TBool)
          , ((Mul,    TInt,  TInt),  TInt) 
          , ((Add,    TInt,  TInt),  TInt) 
          , ((Sub,    TInt,  TInt),  TInt) 
          , ((Div,    TInt,  TInt),  TInt) 
          , ((Mod,    TInt,  TInt),  TInt) 
          , ((Less,   TInt,  TInt),  TBool)
          , ((LessEq, TInt,  TInt),  TBool)
          , ((Eq,     TBool, TBool), TBool)
          , ((Eq,     TInt,  TInt),  TBool)
          ]                                
  

genCommand :: Env -> Gen Command
genCommand env = sized $ \n -> do
    (cmd, _) <- gen n env
    return cmd
  where
    gen n env = do
      frequency $
        [ (1, genSkip)
        , (3, genPrint)
        ] ++
        [ (6, genAssign) | not $ Map.null env ] ++
        concat
        [ [ (4, genSeq)
          , (4, genIf)
          , (4, genWhile)
          ]
        | n > 0
        ]
     where
      gen' = gen (n-1)
      gen2 = gen (n `div` 2)

      arbExpr = do
        t <- arbitrary
        genExpr env t

      genSkip = return (Skip, env)

      genPrint = do
        e <- arbExpr
        return (Print e, env)

      genAssign = do
        (x, t) <- elements $ Map.toList env
        t' <- case t of
                Undef -> arbitrary
                _     -> return t
        e <- genExpr env t'
        return (x := e, Map.insert x t' env)

      genSeq = do
        (c1, env1) <- gen2 env
        (c2, env2) <- gen2 env1
        return (c1 :-> c2, env2)

      genIf = do
        e <- genExpr env TBool
        (c1, env1) <- gen2 env
        (c2, env2) <- gen2 env
        return (If e c1 c2, unionEnv env1 env2)

      genWhile = do
        e <- genExpr env TBool
        (c, env') <- gen' env
        return (While e c, unionEnv env env')

genEnv :: Gen Env
genEnv = do
  xs  <- listOf arbName  -- may contain duplicates
  ts  <- vectorOf (length xs) arbitrary
  return $ Map.fromList $ zip xs ts 
    -- only last binding is used in case of duplicates

genInitEnv :: Gen Env
genInitEnv = do
  xs  <- listOf arbName
  return $ Map.fromList $ zip xs $ repeat Undef

arbCommand :: Gen Command
arbCommand = genCommand =<< genInitEnv

typedShrink :: Command -> [Command]
typedShrink Skip       = []
typedShrink (Print e)  = Skip : [ Print e' | e' <- typedShrinkE e ]
typedShrink (x := e)   = [ x := e' | e' <- typedShrinkE e ]
typedShrink (a :-> b)  =
  [ a' :-> b   | a' <- typedShrink a ] ++
  [ a  :-> b'  | b' <- typedShrink b ]
typedShrink (If c a b) = a : b :
  [ If c' a b  | c' <- typedShrinkE c ] ++
  [ If c a' b  | a' <- typedShrink a  ] ++
  [ If c a b'  | b' <- typedShrink b  ]
typedShrink (While c a) = a :
  [ While c' a  | c' <- typedShrinkE c ] ++
  [ While c a'  | a' <- typedShrink a ]

typedShrinkV :: Value -> [Value]
typedShrinkV (Num n) = [ Num n' | n' <- shrink n ]
typedShrinkV (Bol _) = []
typedShrinkV Wrong   = []

typedShrinkE :: Expr -> [Expr]
typedShrinkE (Val v)      = [ Val v' | v' <- typedShrinkV v ]
typedShrinkE (Var _)      = []
typedShrinkE (Uno op e)   = e : [ Uno op e'
                                | e' <- typedShrinkE e ]
typedShrinkE (Duo op a b) = aOrb ++
  [ Duo op a' b | a' <- typedShrinkE a ] ++
  [ Duo op a b' | b' <- typedShrinkE b ]
  where
    aOrb | op `elem` [Less, Eq, LessEq] = constBools
         | otherwise                    = [a, b]

constBools :: [Expr]
constBools = map (Val . Bol) [False, True]

inferExpr :: Env -> Expr -> Type
inferExpr env (Var x) = case Map.lookup x env of
  Nothing  -> Undef
  Just t   -> t
inferExpr _   (Val v) = case v of
  Num _  -> TInt
  Bol _  -> TBool
  Wrong  -> Undef
inferExpr env (Uno op e)      = inferUno op  (inferExpr env e)
inferExpr env (Duo op e1 e2)  = inferDuo op  (inferExpr env e1)
                                             (inferExpr env e2)

inferUno :: Op1 -> Type -> Type
inferUno op t = case lookup (op, t) unoOps of
  Nothing  -> Undef
  Just t'  -> t'

inferDuo :: Op2 -> Type -> Type -> Type
inferDuo op t1 t2 = case lookup (op, t1, t2) duoOps of
  Nothing  -> Undef
  Just t'  -> t'
