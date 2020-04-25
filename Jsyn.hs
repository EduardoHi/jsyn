{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Jsyn where

import Control.Monad
import qualified Data.Aeson as A
import Data.Bifunctor (bimap, first, second)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import qualified Data.HashMap.Strict as M
import Data.List (find, nub, nubBy, partition, sort)
import Data.Maybe
import Data.Scientific
import Data.Semigroup
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.Vector as V
import GHC.Generics

-- Since jsyn is a tool for Programming by Example,
-- we need to represent an example. An JsonExample is a pair of
-- input and output json values. This datatype can be encoded
-- and decoded from json.
-- The ToJSON and FromJSON instances are just how the
-- Aeson documentation suggests.

data JsonExample = JsonExample
  { input :: A.Value,
    output :: A.Value
  }
  deriving (Generic, Show)

instance A.ToJSON JsonExample where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON JsonExample

decodeJsonExamples :: C.ByteString -> Either String [JsonExample]
decodeJsonExamples content =
  A.eitherDecode content :: Either String [JsonExample]

readJsonExamples :: String -> IO [JsonExample]
readJsonExamples filename = do
  content <- C.readFile filename
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> return v

inferVTexamples :: [JsonExample] -> (ValTy, ValTy)
inferVTexamples examples =
  let t1 = inferArr . V.fromList $ map (inferVT . input) examples
      t2 = inferArr . V.fromList $ map (inferVT . output) examples
   in (t1, t2)

-- From what I've learned following
-- Programming Languages Foundations: https://softwarefoundations.cis.upenn.edu/
-- A definition of a language should have:
-- - Terms and Values
-- - Types

-- Operational semantics:
-- - Reductions

-- Typing Relation
-- - Progress, Preservation
-- - inference rules
-- - terms to types
-- - subtyping

-- ** Values

type Object = A.Object

type Value = A.Value

isString :: Value -> Bool
isString (A.String _) = True
isString _ = False

-- TODO: Boolean blindness in isString it would be better to push that to the type level but idk how

fromString :: Value -> T.Text
fromString (A.String s) = s
fromString v = error $ "value is not a string" ++ show v

-- ** Types

-- A Type represents a subset of possible values, it helps to think about them as
-- abstracting information, and thinking about sets instead of particular values.

-- An important thing to notice is that in this typing scheme we assume that
-- the Arrays are homogenous and they contain the same type in every position,
-- but this is not the case either the json spec, nor in real life jsons.
-- This is a place for improvement for the project, and has room for experimentation.

-- Another important thing to note is that the type of an object is defined
-- by the name of the keys and the type of each value.

data Ty
  = TArrow Ty Ty -- Ty -> Ty
  | TVal ValTy
  deriving (Eq, Show, Ord)

-- TODO: We can create a more sophisticated type system with TRecord and TList
-- that are special cases of when the Object is not being used as a map, and when
-- an array has the same type everywhere
data ValTy
  = TValue -- Supertype of the rest
  | TObject (M.HashMap T.Text ValTy) -- {k1: ValTy1, k2 ValTy2, ...}
  | TArray ValTy -- intersection of: [ValTy1, ValTy2, ...]
  | TString -- "a string"
  | TNumber -- "23"
  | TBool -- true
  | TNull -- null
  deriving (Eq, Show, Ord)

-- The separation between Ty and ValTy is mainly to avoid Arrows inside objects and arrays.
-- Ty represents all possible values in the DSL, and ValTy represents only the values
-- that are valid JSON. Our final program must receive and return a ValTy, but intermediate
-- steps might have Tys.

-- ** Subtyping

-- The Subtyping relation is reflexive and transitive.
-- It also provides the subsumption rule so that the rules can "forget information"

-- ** Type intersection

-- Arrays pose an interesting question since json permits heterogeneous arrays.
-- For example:
-- What should be the type of ["abc", true, 2.3] ?
-- What about [{age: 22, name: "Jim"}, {age: 21, name: "Pam", gpa: 4.2}] ?

-- Also, since the program synthetizer receives a list of input/output examples,
-- we want to infer what's the type over all inputs and outputs.

-- Because of this and js code semantics we define the intersection of two objects
-- as the keys they share and the intersection between their values. For arrays, is the intersection
-- of their values. For the rest of possible value types, the intersection with themselves is
-- reflexive and with the rest is the ValTy TValue.

intersect :: ValTy -> ValTy -> ValTy
intersect a b =
  case (a, b) of
    (TObject ta, TObject tb) -> TObject $ M.intersectionWith intersect ta tb
    (TArray ta, TArray tb) -> TArray $ ta `intersect` tb
    (TString, TString) -> TString
    (TNumber, TNumber) -> TNumber
    (TBool, TBool) -> TBool
    (TNull, TNull) -> TNull
    _ -> TValue

-- intersect can also be thought as the greatest common ancestor in the subtyping tree.

-- The type of a value naturally follows from it's structure:

inferArr :: V.Vector ValTy -> ValTy
inferArr v = case V.length v of
  0 -> TValue
  1 -> V.head v
  _ -> V.foldl1' intersect v

inferVT :: Value -> ValTy
inferVT x =
  case x of
    A.Object o -> TObject $ M.map inferVT o
    A.Array v -> TArray $ inferArr $ V.map inferVT v
    A.String _ -> TString
    A.Number _ -> TNumber
    A.Bool _ -> TBool
    A.Null -> TNull

-- ** DSL

-- This DSL is inspired in jq.

-- but for simplicity I won't add streams, this simplifies the DSL, the type system, inference
-- synthesis and the output programs.

-- Our main terms are Filters and Values.

-- A filter is a function from json values to either a value or a stream,
-- filters are parametrized on the type of streams they might produce
-- but not in the type they receive (they always receive values)
-- this is in part inspired by the robustness principle:
-- "Be conservative in what you send, be liberal in what you accept"

-- In the implementation, filters are a datatype, so that it can be manipulated
-- as data and also be executed with it's corresponding haskell functions.

-- The functions (get, id, ...) etc have a different shape
-- than the natural DSL of the form "id(e), get(e,e), etc" this
-- is because all the expressions have an "implicit argument" that
-- defaults to the current value being evaluated. This limits
-- the number of possible programs but simplifies search.
-- The pipe operator makes it possible to still write interesting
-- programs that could need variables, for example:

-- input = { "nested_obj":{"a":10} }

-- suppose we had that input, and we think we need to write
-- x = get input "nested_obj"
-- return get x "a"

-- but we can just have
-- Pipe (Get "nested_obj") (Get "a")

data Expr
  = Const Value
  | -- /1 arity functions
    Id
  | Keys
  | Elements
  | -- /2 arity functions
    Get Expr
  | Construct [(Expr, Expr)]
  | -- /3 arity functions
    Union Expr Expr
  | Pipe Expr Expr
  deriving (Show, Eq)

fromConstString :: Expr -> T.Text
fromConstString (Const (A.String s)) = s
fromConstString _ = error "not a Const String"

isConstString :: Expr -> Bool
isConstString (Const (A.String _)) = True
isConstString _ = False

instance Ord Expr where
  (Const _) <= Id = True
  Id <= Keys = True
  Keys <= Elements = True
  Elements <= (Get _) = True
  (Get _) <= (Construct _) = True
  (Construct _) <= (Union _ _) = True
  (Union _ _) <= (Pipe _ _) = True
  _ <= _ = False

-- ** Evaluation

-- 'eval' interprets a filter against a value and returns a value
-- this can be seen as the "Haskell" interpretation of the DSL
-- to be used during the search to find the correct programs.
-- This can also be seen as the spec of the semantics of the DSL,
-- since I'm not going to formalize it with math.

type EvalRes = Either String Value

eval :: Expr -> Value -> EvalRes
eval x val = case x of
  -- partially evaluated
  Id -> pure val
  Const v -> pure v
  -- partially evaluated
  Get f -> get val f
  Construct fs -> construct val fs
  Pipe f g -> pipe val f g
  -- partially evaluated
  Keys -> keys val
  -- partially evaluated
  Elements -> elements val
  Union f g -> union val f g

keys :: Value -> EvalRes
keys (A.Object o) =
  Right . A.Array . V.fromList $ map A.String $ M.keys o
keys val =
  Left $ "called keys of value: " ++ show val ++ "that is not an object"

elements :: Value -> EvalRes
elements (A.Object o) =
  Right . A.Array . V.fromList $ M.elems o
elements val =
  Left $ "called elems of value: " ++ show val ++ "that is not an object"

-- | val : the value from eval
-- | f   : the filter that evaluated returns the key for the object val

-- 1. eval the filter with the current value
-- if it is a single value:
-- 2. if it's
get :: Value -> Expr -> EvalRes
get val f =
  eval f val >>= get'
  where
    get' :: Value -> EvalRes
    get' valKey =
      case valKey of
        A.String k -> getVal k
        _ -> Left "Can't use a non-string as key"
    getVal :: T.Text -> EvalRes
    getVal v = case val of
      A.Object o ->
        maybe
          (Left . T.unpack $ "key: \"" <> v <> "\" not found in object: " <> T.pack (show o))
          Right
          (v `M.lookup` o)
      _ -> Left $ "value: " ++ show val ++ "is not an object"

construct :: Value -> [(Expr, Expr)] -> EvalRes
construct val fs =
  let kys = map (flip eval val . fst) fs
      vls = map (flip eval val . snd) fs
   in construct' kys vls
  where
    construct' ks vs =
      case (partitionEithers ks, partitionEithers vs) of
        (([], rks), ([], rvs)) -> build rks rvs
        ((lks, _), (lvs, _)) -> Left $ unlines lks ++ "\n" ++ unlines lvs
    build ks vs =
      if all isString ks
        then Right . A.Object . M.fromList $ zip (map fromString ks) vs
        else Left $ " keys have a value that is not a string: " ++ show (head $ takeWhile isString ks)

pipe :: Value -> Expr -> Expr -> EvalRes
pipe v f g =
  eval f v >>= eval g

union :: Value -> Expr -> Expr -> EvalRes
union val f g = do
  l <- eval f val
  r <- eval g val
  case (l, r) of
    (A.Object o1, A.Object o2) -> Right $ A.Object (o1 `M.union` o2)
    (_, A.Object _) -> Left "Left hand side of union is not an Object"
    (A.Object _, _) -> Left "Right hand side of union is not an Object"
    (_, _) -> Left "union is not with objects"

-- A Program is what we finally want to have, a function wrapping the filter and returning it:
-- ```js
-- function program(obj) {
--   return filter(obj)
-- }
-- ```

newtype Program = Program {programBody :: Expr}
  deriving (Show, Eq)

toJS :: Program -> T.Text
toJS Program {programBody = body} =
  T.unlines
    [ "function program(obj) {",
      "return " <> toJSInline "obj" body,
      "}"
    ]

-- toJSInline converts Exprs to Javascript code

-- Since the Exprs have an implicit arg, when we print we need to "thread"
-- that arg down the tree, and up in the assignments. in the case of the
-- inlined, the argument is always the same.

toJSInline :: T.Text -> Expr -> T.Text
toJSInline s x =
  case x of
    (Const v) -> E.decodeUtf8 . C.toStrict $ A.encode v
    Id -> s
    Keys -> "Object.keys(" <> s <> ")"
    Elements -> "Object.values(" <> s <> ")"
    (Get f) ->
      if isConstString f && isValidAsKey (fromConstString f)
        then s <> "." <> fromConstString f
        else s <> "[" <> f' <> "]"
      where
        f' = toJSInline s f
    (Construct fs) ->
      "{" <> inlinePairs <> "}"
      where
        inlinePairs = T.intercalate ", " $ map go fs
        go (k, v) = treatKey k <> ":" <> toJSInline s v
        treatKey kstr =
          if isConstString kstr && isValidAsKey (fromConstString kstr)
            then toJSInline s kstr
            else "[" <> toJSInline s kstr <> "]"
    (Union f g) -> "Object.assign(" <> f' <> ", " <> g' <> ")"
      where
        f' = toJSInline s f
        g' = toJSInline s g
    (Pipe f g) -> f' <> "(" <> g' <> ")"
      where
        f' = toJSInline s f
        g' = toJSInline s g

-- A valid string for a key does not have spaces or double quotes
isValidAsKey st = not $ T.any (\x -> x == ' ' || x == '"') st

-- Enumerative Search Algorithm.

-- 1. start with set of terminals
--    ps is the set of possible programs

-- 2. Increase the possible trees
-- ps = grow ps
-- grow :: Set Expr -> Set Expr
-- grows plist
-- returns the list of all trees generated by
-- taking a non-terminal and adding nodes from plist as children

-- 3. if any p in ps is consistent with the examples, return p else repeat
--   consistent means every input is correctly mapped to it's output

-- we do this with a bounded size to avoid infinite trees

-- For now, the only initial constants are the values in
-- the keys of the object
-- TODO: when extending the domain, extend this to extract more values
extractConst :: Value -> [Expr]
extractConst x =
  case x of
    (A.Object o) -> map (Const . A.String) $ M.keys o
    _ -> []

objWidth :: Value -> Int
objWidth (A.Object o) = max (M.size o) $ maximum $ map objWidth $ M.elems o
objWidth _ = 0

botupSearch :: Int -> [JsonExample] -> Maybe Expr
botupSearch bound examples =
  let ps = nub $ concatMap (extractConst . input) examples
      -- 1 arity functions are terminals too
      ps' = ps ++ [Id, Keys, Elements]
      width = maximum $ map (objWidth . output) examples
   in searchStep bound width ps' examples

searchStep :: Int -> Int -> [Expr] -> [JsonExample] -> Maybe Expr
searchStep 0 width _ _ = Nothing
searchStep bound width ps examples =
  let ps' = grow width ps
   in case find (consistent examples) ps' of
        Nothing -> searchStep (bound -1) width (ps ++ ps') examples
        Just consistentProgram -> Just consistentProgram

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

grow :: Int -> [Expr] -> [Expr]
grow width nodes =
  -- n +  n^2 C width  + n^2 + n^2
  growGet ++ growConstruct ++ growUnion ++ growPipe
  where
    growGet = map Get nodes
    -- n^2 in size
    combs = cartProd nodes nodes
    -- all possible combinations up to width
    -- of the pairs of elements
    -- this is extremely slow but exhaustive
    growConstruct = map Construct $ combinations width combs
    growUnion = map (uncurry Union) combs
    growPipe = map (uncurry Pipe) combs

-- All unique combinations of size n
-- > combinations 2 [1,2,3]
-- [[1,2],[1,3]]
combinations n xs =
  nubBy (\x y -> sort x == sort y)
    $
    -- this line would make it up to size n
    -- n <- [1..(min n $ length xs)]
    filter ((n ==) . length . nub)
    $ mapM (const xs) [1 .. n]

consistent :: [JsonExample] -> Expr -> Bool
consistent examples expr =
  all go examples
  where
    go JsonExample {input = i, output = o} =
      case eval expr i of
        Left s -> False -- Left is an error during evaluation
        Right r -> r == o

-- Enumeration of types for now
data ExprT
  = TGet
  | TConstruct
  deriving (Show, Read, Eq, Ord)

-- forward is a single step only with types
-- it answers the following question:
-- given a type and an Expression, what is the type of the resulting Value?

-- | t = TObject (fromList [("age",TNumber),("name",TString)])
-- >> forward t (Get "")
-- >> [(Get (Const (String "age")),TNumber),(Get (Const (String "name")),TString)]
forward :: ValTy -> ExprT -> [(Expr, ValTy)]
forward t e =
  case e of
    TGet ->
      case t of
        TObject o -> map (first $ Get . Const . A.String) $ M.toList o
        _ -> []
    -- TKeys ->
    --   case t of
    --     TObject o -> [(Keys, TArray TString)]
    --     _ -> []
    -- TElements ->
    --   case t of
    --     TObject o -> [(Elements, TArray $ inferArr $ V.fromList $ M.elems o)]
    --     _ -> []
    _ -> []

-- TODO: make forward a total function

type THole = (String, ValTy)

-- | from a type get expressions with holes that can give current type
-- >>
-- [("e1",TNumber),("e2",TNumber)]
backward :: ValTy -> ExprT -> [THole]
backward t e =
  case e of
    TConstruct ->
      case t of
        TObject o -> zip idents $ M.elems o
        _ -> []
  where
    idents = map (("e" ++) . show) [1 ..]

unify :: ValTy -> ValTy -> [Expr]
unify = undefined

-- Inductive Generation Search without deduction
--
--

-- hypothesis expression
data HExpr
  = HGet T.Text
  | HConstruct [(T.Text, HExpr)]
  | HPipe HExpr HExpr
  | Hole ValTy
  deriving (Show, Eq, Ord)

hExprToExpr :: HExpr -> Expr
hExprToExpr h =
  case h of
    (HGet t) -> Get . Const . A.String $ t
    (HConstruct exps) ->
      Construct $ map (bimap (Const . A.String) hExprToExpr) exps
    (HPipe exp1 exp2) ->
      Pipe (hExprToExpr exp1) (hExprToExpr exp2)
    (Hole _) -> error "can't convert an open hypothesis to an expression"

isClosed :: HExpr -> Bool
isClosed (HGet _) = True
isClosed (HConstruct exps) = all isClosed $ map snd exps
isClosed (HPipe exp1 exp2) = isClosed exp1 && isClosed exp2
isClosed (Hole h) = False

indGenSearch :: [JsonExample] -> Maybe Program
indGenSearch examples =
  -- search a program of the form:
  -- \x . e
  -- x : t1, e : t2
  msum $ step t1 examples hypotheses
  where
    (t1, t2) = inferVTexamples examples
    hypotheses = inductiveGen (t1, t2)

step :: ValTy -> [JsonExample] -> [HExpr] -> [Maybe Program]
step t1 examples hs =
-- split closed and open hypotheses
-- for every closed hypothesis, check if any is consistent, if it is return that one
-- else, for each open hypotheses generate more closed/open hypotheses
-- and recursively search them
  let (closedhs, openhs) = partition isClosed hs
  in
    case find (consistent examples . hExprToExpr) closedhs of
      Just hConsistent -> [Just . Program . hExprToExpr $ hConsistent]
      Nothing -> case openhs of
                   [] -> [Nothing]
                   hs' -> programs
                     where
                       expandedHypotheses :: [HExpr]
                       expandedHypotheses = concatMap (expand t1) openhs
                       programs :: [Maybe Program]
                       programs = step t1 examples expandedHypotheses

-- given an open hypothesis, return all the hypotheses it generates
expand :: ValTy -> HExpr -> [HExpr]
expand t1 h =
  case h of
    HConstruct exps ->
      let kys = map fst exps
          -- quadratic on the number of generated hypotheses by inductiveGen (t1, t)
          allCombinations = mapM ((\(Hole t) -> inductiveGen (t1, t)) . snd) exps
       in map (HConstruct . zip kys) allCombinations
    HPipe (Hole th1) (Hole t2) ->
      do
        exp1 <- inductiveGen (t1, th1)
        exp2 <- inductiveGen (th1, t2)
        return $ HPipe exp1 exp2
    HPipe e1 e2 -> []
    h -> error $ show h

-- [
--   HPipe (Hole TNumber) (Hole TString),
--   HPipe (Hole TBool) (Hole TString),
--   HPipe (Hole TString) (Hole TString),
--   HPipe (Hole (TObject (fromList [("online",TBool),("name",TString),("host",TString)])))
--         (Hole TString)
-- ]

-- TODO: Replace pair of ValTy with TArr when generalizing to multiple args ?

-- | from an arrow type, return a stream of hypothesis compatible with those types
inductiveGen :: (ValTy, ValTy) -> [HExpr]
inductiveGen (t1, t2) =
  getHs ++ constructHs ++ pipeHs
  where
    -- construct hypotheses
    constructHs =
      case t2 of
        TObject o ->
          [HConstruct $ map (second Hole) $ M.toList o]
        _ -> []
    -- get hypotheses, note how this generates hypotheses without holes
    getHs =
      case t1 of
        TObject o ->
          -- from all the fields of t1 filter the ones that are equal to t2
          -- and return their gets
          map (HGet . fst) $ filter ((t2 ==) . snd) $ M.toList o
        _ -> []
    pipeHs =
      let types =
            [TNumber, TBool, TString]
              ++ ( case t1 of
                     TObject o -> M.elems o
                     TArray a -> [a]
                     _ -> []
                 )
          holes = map Hole $ nub types
       in -- quadratic in number of holes
          HPipe <$> holes <*> [Hole t2]

