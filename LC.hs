{-# LANGUAGE OverloadedStrings #-}

-- |
module LC where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Aeson as A
import Data.Bifunctor (bimap, first, second)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.Text as T

type Value = A.Value

--
-- Rework of the DSL as a simple lambda calculus extension
--

newtype Variable = Variable T.Text
  deriving (Show, Eq)

variable :: T.Text -> Expr
variable s = Var $ Variable s

-- LVar, LVal and Lambda are the core Lambda Calculus,
-- Get and Construct are extensions
data Expr
  = Var Variable -- variable literal
  | Val Value -- a value
  | Lam Variable Expr -- lambda abstraction
  | App Expr Expr -- function application
  | Get T.Text Expr -- getter of object
  | Con [(T.Text, Expr)] -- constructor of object
  | Map Expr Expr -- mapping a function over a list
  deriving (Show, Eq)

-- toplevel evaluation of a program
-- equivalent to (\x -> prg) val
-- where prg is the synthetized body in terms of x
evalPrg :: Expr -> Value -> Value
evalPrg prg val =
  case eval (App prg (Val val)) of
    Val v -> v
    x -> error $ "evaluation not ended in a normal form: " <> show x

-- type EvalRes = Either T.Text Expr
type EvalRes = Expr

eval :: Expr -> Expr
eval e =
  case e of
    Get k e ->
      case eval e of
        Val (A.Object o) ->
          case M.lookup k o of
            Nothing -> error . T.unpack $ "key: " <> k <> " not found"
            Just x -> Val x
        e' -> error $ show e' <> "is not an object"
    Con es ->
      let (ks, vs) = unzip es
          vs' = map
       in Val . A.Object $ M.fromList $ zip ks (map evalVal vs)
      where
        evalVal v =
          case eval v of
            Val v' -> v'
            v'' -> error $ show v'' <> " is not a value, when building object"
    App fun arg ->
      case eval fun of
        Lam var body ->
          -- substitute `var` with value of evaluating `arg` in `body`
          eval $ subst var (eval arg) body
        -- here I will add  the other builtin but not primitive functions (like +,-, etc)
        f -> error $ show f <> "cannot be applied because it is not a function"
    -- Var, Val and Lam evaluate to themselves
    e -> e

-- | substitute all instances of `var` with `val` in `exp`
subst :: Variable -> Expr -> Expr -> Expr
subst var val exp =
  case exp of
    Var v
      | v == var -> val
    Get k e ->
      Get k (subst var val e)
    Con xs ->
      Con $ fmap (second (subst var val)) xs
    Lam v body
      | v /= var && v `notElem` freeVars val -> Lam v (subst var val body)
      | otherwise -> Lam v body
    App x y ->
      App (subst var val x) (subst var val y)
    e -> e

freeVars :: Expr -> [Variable]
freeVars e =
  case e of
    Var v -> [v]
    Lam v body -> filter (v /=) $ freeVars body
    e -> freeVars e

----------------------------------------------------------------------
--                               Types                              --
----------------------------------------------------------------------

data Ty
  = TArrow Ty Ty
  | TVal ValTy
  deriving (Show, Eq)

infixr 9 `TArrow`

isTVal (TVal _) = True
isTVal _ = False

fromTVal (TVal v) = v
fromTVal _ = error "not a TVal"

type TObject = M.HashMap T.Text ValTy

data ValTy
  = TValue -- Supertype of the rest
  | TObject TObject -- {k1: ValTy1, k2 ValTy2, ...}
  | TArray ValTy -- intersection of: [ValTy1, ValTy2, ...]
  | TString -- "a string"
  | TNumber -- "23"
  | TBool -- true
  | TNull -- null
  deriving (Show, Eq)

isTObj (TObject _) = True
isTObj _ = False

fromTObj (TObject o) = o
fromTObj _ = error "not an object"

-- differences to Expr:
-- 1. Variable node has a type annotation
-- 2. there is no value node
--

data HExpr
  = HVar Variable Ty -- variable literal annotated with a type
  | HLam Variable HExpr -- lambda abstraction
  | HApp HExpr HExpr -- function application
  | HGet T.Text HExpr -- getter of object
  | HCon [(T.Text, HExpr)] -- constructor of object
  | HMap HExpr HExpr -- mapping a function over a list
  deriving (Show, Eq)

-- go from a hypothesis to a concrete Expression
concretize :: HExpr -> Expr
concretize e =
  case e of
    (HVar v _) -> Var v
    (HLam v e) -> Lam v $ concretize e
    (HApp f a) -> App (concretize f) (concretize a)
    (HGet k e) -> Get k (concretize e)
    (HCon exp) -> Con $ map (second concretize) exp
    (HMap e l) -> Map (concretize e) (concretize l)

-- | a Hypotesis is an Expression with 0 or more free variables
-- if it's 0 it's said to be closed, otherwise it is open.
-- if it is open, then every free variable has a type associated.

-- data Hypothesis = Hypothesis [(Variable, Ty)] Expr
type Hypothesis = HExpr

type Context = [(Variable, Ty)]
-- isClosed :: Hypothesis -> Bool
-- isClosed (Hypothesis [] _) = True
-- isClosed _ = False

-- isOpen :: Hypothesis -> Bool
-- isOpen = not . isClosed

matching :: (b -> Bool) -> [(a,b)] -> [(a,b)]
matching pred = filter (pred . snd)

genClose :: Ty -> Context -> [Hypothesis]
genClose t ctx =
  vars ++ gets
  where
    -- matching :: (b -> Bool) -> [(Variable,b)]
    -- matching pred =
    --   filter (pred . snd) ctx
    vars =
      map (uncurry HVar) $ matching (t==) ctx
    gets =
      case t of
        TVal valt ->
          let
            valtys :: [(Variable,ValTy)]
            valtys = map (second fromTVal) $ matching isTVal ctx
            objs :: [(Variable, ValTy)]
            objs = matching isTObj valtys
            o2gets :: (Variable, ValTy) -> [Hypothesis]
            o2gets (v,o) = map (\(k,_) -> HGet k (HVar v (TVal o)))
                           $ matching (valt==)
                           $ M.toList (fromTObj o)
          in concatMap o2gets objs
        _ -> []



type Search = State [Variable]

-- | given a type Ty generate all possible open hypothesis
-- that 'fit' it
open :: Ty -> Search [Hypothesis]
open t = do
  x <- fresh 1
  undefined
  -- let gH = [HGet (HVar x (TObject $ M.fromList [("")])) ""]
  -- let mH = case t of
  --       TArray a -> [HMap (HVar x (TVal ))]




-- | return a list of `c` fresh variable names
fresh :: Int -> Search [Variable]
fresh c = do
  xs <- get
  put $ drop c xs
  return $ take c xs

letters :: [Variable]
letters = map (Variable . T.pack) ([1 ..] >>= flip replicateM ['a' .. 'z'])
