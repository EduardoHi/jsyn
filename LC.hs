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

isTArrow (TArrow _ _) = True
isTArrow _ = False

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
-- 1. there's a hole node that represents a free variable
-- 2. there is no value node
--

data HExpr
  = HHole Ty -- variable literal annotated with a type
  | HVar Variable -- variable literal annotated with a type
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
    (HVar v) -> Var v
    (HHole t) -> error $ "Can't concretize expression with hole: " <> show t
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

matching :: (b -> Bool) -> [(a, b)] -> [(a, b)]
matching pred = filter (pred . snd)

genClose :: Ty -> Context -> [Hypothesis]
genClose t ctx =
  genVars t ctx
    ++ genGets t ctx
    ++ genCons t ctx
    ++ genApps t ctx
  where
    genVars t ctx =
      map (HVar . fst) $ matching (t ==) ctx

-- | generate all possible expressions that return type t.
-- if t is not an object it returns the empty list
-- if t is an object, it returns all possible combinations
-- of current variables in context.
--
-- e.g. if context has [("a", TString), ("b", TString), ("c", TNumber), ("d", TNumber)]
-- and we want to fill t = {k: TString, k2: TNumber}
-- it returns [ {k:a, k2:c}, {k:a, k2:d}, {k:b, k2:c}, {k:b, k2:d} ]
-- as possible expressions
genCons :: Ty -> Context -> [Hypothesis]
genCons t ctx =
  case t of
    TVal (TObject o) ->
      let -- get only the variables from ctx that return values
          valtys :: [(Variable, ValTy)]
          valtys =
            map (second fromTVal) $
              matching isTVal ctx
          -- from a key and value type, generate a stream of all
          -- pairs of that key and a correct type expression
          go :: (T.Text, ValTy) -> [(T.Text, HExpr)]
          go (k, vt) = do
            vts <- map (HVar . fst) $ matching (vt ==) valtys
            return (k, vts)
       in map HCon $ mapM go $ M.toList o
    _ -> []

genApps :: Ty -> Context -> [Hypothesis]
genApps t ctx =
  let functions = matching isTArrow ctx
      fsreturnT = matching (\(TArrow a b) -> b == t) functions
   in do
        -- since it is in the list monad, this generates all possible combinations
        -- of functions f and arguments varA
        (f, TArrow a _) <- fsreturnT
        (varA, _) <- matching (a ==) ctx
        return $ HApp (HVar f) (HVar varA)

-- | Generate all Get expressions that match type t with all variables in context
-- from the variables that are objects, we generate all possible get expressions that
-- can return t, including nested expressions.
-- e.g.
-- if y : {z: TString, x {b: TString}} is the context,
-- y.z and y.x.b will be generated
genGets :: Ty -> Context -> [Hypothesis]
genGets t ctx =
  case t of
    TVal valt ->
      let valtys :: [(Variable, ValTy)]
          valtys =
            matching isTObj
              $ map (second fromTVal)
              $ matching isTVal ctx
          o2gets :: (Variable, ValTy) -> [Hypothesis]
          o2gets (v, o) =
            map (foldr HGet (HVar v)) $
              pathsToT o valt
       in concatMap o2gets valtys
    _ -> []

-- | all the paths that are of type t inside object o.
-- eg
-- > y = (TObject (M.fromList [("x", (TObject (M.fromList [("b", TString)]))), ("z", TString)]))
-- > pathsToT y
-- [["z"],["x","b"]]
pathsToT :: ValTy -> ValTy -> [[T.Text]]
pathsToT o t =
  let keyTypes = M.toList (fromTObj o)
      inmediate = map (\(k, to) -> [k]) $ matching (t ==) keyTypes
      recursive =
        map (\(k, to) -> pathsToT to t >>= (k :)) $ matching isTObj keyTypes
   in inmediate ++ recursive

type Search = State [Variable]

-- | given a type Ty generate all possible open hypothesis
-- that 'fit' it
genOpen :: Ty -> [Hypothesis]
genOpen =
  genOpenCon

genOpenCon :: Ty -> [Hypothesis]
genOpenCon t = 
  case t of
    TVal (TObject o) ->
      let kts = M.toList o
      in [HCon $ map (second $ HHole . TVal) kts]
    _ -> []

-- | return a list of `c` fresh variable names
fresh :: Int -> Search [Variable]
fresh c = do
  xs <- get
  put $ drop c xs
  return $ take c xs

letters :: [Variable]
letters = map (Variable . T.pack) ([1 ..] >>= flip replicateM ['a' .. 'z'])
