{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Jsyn (JsonExample, ValTy, SynthRes(..), runSynth, toJS) where

import JsonExample 
import Control.Exception (evaluate)
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (bimap, first, second)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as M
import Data.List (find, nub, partition)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.Vector as V
import System.Timeout (timeout)

-- Since jsyn is a tool for Programming by Example,
-- we need to represent an example. A JsonExample is a pair of
-- input and output json values. This datatype can be encoded
-- and decoded from json.
-- The ToJSON and FromJSON instances are implemented following the Aeson documentation.


inferVTexamples :: [JsonExample] -> (ValTy, ValTy)
inferVTexamples examples =
  let t1 = inferArr . V.fromList $ map (inferVT . input) examples
      t2 = inferArr . V.fromList $ map (inferVT . output) examples
   in (t1,t2)

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

-- A json value is exactly the same as the Aeson Json datatype.
type Value = A.Value

kmToHashMap :: KM.KeyMap a -> M.HashMap T.Text a
kmToHashMap = M.fromList . map (first K.toText) . KM.toList

-- ** Types

-- A Type represents a subset of possible values, it helps to think about them as
-- abstracting information, and thinking about sets instead of particular values.

-- An important thing to notice is that in this typing scheme we assume that
-- the Arrays are homogenous and they contain the same type in every position,
-- but this is not the case either the json spec, nor in real life jsons.
-- This is a place for improvement for the project, and has room for experimentation.

-- TODO: Support Heterogeneous Arrays

-- Another important thing to note is that the type of an object is defined
-- by the name of the keys and the type of each value.

data Ty
  = TArrow Ty Ty -- Ty -> Ty
  | TVal ValTy
  deriving (Eq, Show, Ord)

-- Since it is (->) type
infixr 9 `TArrow`

tarrow :: ValTy -> ValTy -> Ty
v `tarrow` w = TVal v `TArrow` TVal w

prettyTy :: Ty -> T.Text
prettyTy t = case t of
  TArrow a b -> prettyTy a <> " -> " <> prettyTy b
  TVal a -> prettyValTy a

prettyValTy :: ValTy -> T.Text
prettyValTy val = case val of
  TValue -> "Value"
  TObject o -> "{" <> inside <> "}"
    where
      inside =
        T.intercalate ", " $ map (\(k, v) -> k <> ": " <> prettyValTy v) $ M.toList o
  TArray a -> "[" <> prettyValTy a <> "]"
  TString -> "String"
  TNumber -> "Number"
  TBool -> "Bool"
  TNull -> "Null"

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

-- https://flow.org/en/docs/types/intersections/
-- intersection of object types `merges` fields in both objects
--
-- https://flow.org/en/docs/types/unions/
-- union of object types needs a single property to differentiate between both
--
-- intersection types refer to a type that is type a *and* type b
-- while union refers to a type that is type a *xor* type b

typeUnion :: ValTy -> ValTy -> ValTy
typeUnion a b =
  case (a, b) of
    (TObject ta, TObject tb) -> TObject $ M.intersectionWith typeUnion ta tb
    (TArray ta, TArray tb) -> TArray $ ta `typeUnion` tb
    (TString, TString) -> TString
    (TNumber, TNumber) -> TNumber
    (TBool, TBool) -> TBool
    (TNull, TNull) -> TNull
    _ -> TValue

-- The type of a value naturally follows from it's structure:

inferArr :: V.Vector ValTy -> ValTy
inferArr v = case V.length v of
  0 -> TValue
  1 -> V.head v
  _ -> V.foldl1' typeUnion v

inferVT :: Value -> ValTy
inferVT x =
  case x of
    A.Object o -> TObject $ M.map inferVT (kmToHashMap o)
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
  | Flatten
  | ToList
  | -- /2 arity functions
    Get Expr
  | EMap Expr
    -- boolean operators
  | Equal Expr Expr
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Construct [(Expr, Expr)]
  | -- /3 arity functions
    Union Expr Expr
  | Pipe Expr Expr
  | LConcat Expr Expr
  deriving (Show, Eq)

fromConstString :: Expr -> T.Text
fromConstString (Const (A.String s)) = s
fromConstString _ = error "not a Const String"

isConstString :: Expr -> Bool
isConstString (Const (A.String _)) = True
isConstString _ = False

-- ** Evaluation

-- 'eval' interprets a filter against a value and returns a value
-- this can be seen as the "Haskell" interpretation of the DSL
-- to be used during the search to find the correct programs.
-- This can also be seen as the spec of the semantics of the DSL,
-- since I'm not going to formalize it with math.

type EvalRes = Either String Value

type EvalM a = Either String a

expectBool :: String -> Value -> EvalM Bool
expectBool ctx = \case
  A.Bool b -> Right b
  other -> Left $ ctx <> " expected a boolean but got: " <> show other

expectArray :: String -> Value -> EvalM (V.Vector Value)
expectArray ctx = \case
  A.Array arr -> Right arr
  other -> Left $ ctx <> " expected an array but got: " <> show other

expectObject :: String -> Value -> EvalM (KM.KeyMap Value)
expectObject ctx = \case
  A.Object obj -> Right obj
  other -> Left $ ctx <> " expected an object but got: " <> show other

expectString :: String -> Value -> EvalM T.Text
expectString ctx = \case
  A.String s -> Right s
  other -> Left $ ctx <> " expected a string but got: " <> show other

eval :: Expr -> Value -> EvalRes
eval x val = case x of
  -- partially evaluated
  Id -> pure val
  Const v -> pure v
  -- partially evaluated
  Get f -> get val f
  Equal l r -> equal val l r

  Not e -> bnot val e
  And l r -> band val l r
  Or l r -> bor val l r

  Construct fs -> construct val fs
  Pipe f g -> pipe val f g
  -- partially evaluated
  Keys -> keys val
  -- partially evaluated
  Elements -> elements val
  Union f g -> union val f g
  EMap f -> evalmap val f
  LConcat l r -> evalconcat val l r
  ToList -> pure $ A.Array $ V.singleton val
  Flatten -> flatten val

-- piggy back on Aeson's value equality
equal :: Value -> Expr -> Expr -> EvalRes
equal val l r = do
  l' <- eval l val
  r' <- eval r val
  pure $ A.Bool $ l' == r'

bnot :: Value -> Expr -> EvalRes
bnot val e = do
  b <- eval e val >>= expectBool "not"
  pure $ A.Bool $ not b

band :: Value -> Expr -> Expr -> EvalRes
band val l r = do
  bl <- eval l val >>= expectBool "Left hand side of &&"
  br <- eval r val >>= expectBool "Right hand side of &&"
  pure $ A.Bool $ bl && br

bor :: Value -> Expr -> Expr -> EvalRes
bor val l r = do
  bl <- eval l val >>= expectBool "Left hand side of ||"
  br <- eval r val >>= expectBool "Right hand side of ||"
  pure $ A.Bool $ bl || br

keys :: Value -> EvalRes
keys (A.Object o) =
  Right . A.Array . V.fromList $ map (A.String . K.toText) $ KM.keys o
keys val =
  Left $ "called keys on value: " ++ show val ++ " that is not an object"

elements :: Value -> EvalRes
elements (A.Object o) =
  Right . A.Array . V.fromList $ KM.elems o
elements val =
  Left $ "called elems on value: " ++ show val ++ " that is not an object"

-- | val : the value from eval
-- | f   : the filter that evaluated returns the key for the object val

-- 1. eval the filter with the current value
-- if it is a single value:
-- 2. if it's
get :: Value -> Expr -> EvalRes
get val f = do
  key <- eval f val >>= expectString "get"
  obj <- expectObject "get" val
  maybe
    (Left . T.unpack $ "key: \"" <> key <> "\" not found in object: " <> T.pack (show obj))
    Right
    (KM.lookup (K.fromText key) obj)

construct :: Value -> [(Expr, Expr)] -> EvalRes
construct val fields = do
  pairs <- forM fields $ \(kExpr, vExpr) -> do
    key <- eval kExpr val >>= expectString "construct field key"
    value <- eval vExpr val
    pure (K.fromText key, value)
  pure . A.Object $ KM.fromList pairs

pipe :: Value -> Expr -> Expr -> EvalRes
pipe v f g =
  eval f v >>= eval g

union :: Value -> Expr -> Expr -> EvalRes
union val f g = do
  left <- eval f val >>= expectObject "Left hand side of union"
  right <- eval g val >>= expectObject "Right hand side of union"
  pure $ A.Object (KM.union left right)

evalmap :: Value -> Expr -> EvalRes
evalmap val f = do
  arr <- expectArray "map" val
  mapped <- V.mapM (eval f) arr
  pure $ A.Array mapped

evalconcat :: Value -> Expr -> Expr -> EvalRes
evalconcat val l r = do
  left <- eval l val >>= expectArray "Left hand side of concat"
  right <- eval r val >>= expectArray "Right hand side of concat"
  pure $ A.Array $ V.concat [left, right]

flatten :: Value -> EvalRes
flatten val = do
  outer <- expectArray "flatten" val
  inner <- V.mapM (expectArray "flatten expects each element to be an array") outer
  let flattened = V.concat (V.toList inner)
  pure $ A.Array flattened

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
    [ "function program(x) {",
      "    return " <> toJSInline "x" body,
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
    (Equal l r) -> toJSInline s l <> " == " <> toJSInline s r
    (Not e) -> "!" <> toJSInline s e
    (And l r) -> toJSInline s l <> " && " <> toJSInline s r
    (Or l r) -> toJSInline s l <> " || " <> toJSInline s r
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
    (Pipe f g) -> toJSInline f' g
      where
        f' = toJSInline s f
    (EMap e) -> s <> ".map( x => { return " <> toJSInline "x" e <> "; })"
    (LConcat l r) -> toJSInline s l <> " + " <> toJSInline s r
    ToList -> "[" <> s <> "]"
    Flatten -> "(" <> s <> ").flat()"

-- A valid string for a key does not have spaces or double quotes
isValidAsKey :: T.Text -> Bool
isValidAsKey st = not $ T.any (\x -> x == ' ' || x == '"') st

consistent :: [JsonExample] -> Expr -> Bool
consistent examples expr =
  all go examples
  where
    go JsonExample {input = i, output = o} =
      case eval expr i of
        Left _ -> False -- Left is an error during evaluation
        Right r -> r == o

-----------------------------------------------------------------------
--           Inductive Generation Search without deduction           --
-----------------------------------------------------------------------

-- hypothesis expression
data HExpr
  = HGet T.Text
  | HEqual HExpr HExpr
  | HNot HExpr
  | HAnd HExpr HExpr
  | HOr HExpr HExpr
  | HConstruct [(T.Text, HExpr)]
  | HPipe HExpr HExpr ValTy
  | HMap HExpr ValTy
  | HConcat HExpr HExpr
  | HToList
  | HFlatten
  | Hole Ty
  deriving (Show, Eq, Ord)

isHole :: HExpr -> Bool
isHole (Hole _) = True
isHole _ = False

debugHExpr :: [HExpr] -> IO ()
debugHExpr =
  mapM_ (print . prettyHExpr)

prettyHExpr :: HExpr -> T.Text
prettyHExpr e =
  case e of
    (HGet k) -> "get(" <> k <> ")"
    (HConstruct ps) -> "{" <> inside <> "}"
      where
        inside =
          T.intercalate ", " $ map (\(k, v) -> k <> ": " <> prettyHExpr v) ps
    (HPipe e1 e2 _) -> prettyHExpr e1 <> " |> " <> prettyHExpr e2
    (HMap arg _) -> "map(" <> prettyHExpr arg <> ")"
    (HConcat a b) -> prettyHExpr a <> " <> " <> prettyHExpr b
    HToList -> "toList"
    HFlatten -> "flatten"
    HEqual l r -> prettyHExpr l <> " == " <> prettyHExpr r
    HNot e' -> "!" <> prettyHExpr e'
    HAnd l r -> prettyHExpr l <> " && " <> prettyHExpr r
    HOr l r -> prettyHExpr l <> " || " <> prettyHExpr r
    (Hole t) -> "hole:" <> prettyTy t

hExprToExpr :: HExpr -> Expr
hExprToExpr h =
  case h of
    (HGet t) -> Get . Const . A.String $ t
    (HEqual l r) -> Equal (hExprToExpr l) (hExprToExpr r)
    (HNot e) -> Not (hExprToExpr e)
    (HAnd l r) -> And (hExprToExpr l) (hExprToExpr r)
    (HOr l r) -> Or (hExprToExpr l) (hExprToExpr r)
    (HConstruct es) ->
      Construct $ map (bimap (Const . A.String) hExprToExpr) es
    (HPipe e1 e2 _) ->
      Pipe (hExprToExpr e1) (hExprToExpr e2)
    (HConcat e1 e2) ->
      LConcat (hExprToExpr e1) (hExprToExpr e2)
    HToList -> ToList
    HFlatten -> Flatten
    (HMap e _) -> EMap $ hExprToExpr e
    (Hole _) -> error "can't convert an open hypothesis to an expression"

isClosed :: HExpr -> Bool
isClosed (HGet _) = True
isClosed (HEqual l r) = isClosed l && isClosed r
isClosed (HNot e) = isClosed e
isClosed (HOr l r) = isClosed l && isClosed r
isClosed (HAnd l r) = isClosed l && isClosed r

isClosed (HConstruct es) = all (isClosed . snd) es
isClosed (HPipe e1 e2 _) = isClosed e1 && isClosed e2
isClosed (HMap e _) = isClosed e
isClosed (HConcat e1 e2) = isClosed e1 && isClosed e2
isClosed HToList = True
isClosed HFlatten = True
isClosed (Hole _) = False

hSize :: HExpr -> Int
hSize (HGet _) = 1
hSize (HEqual l r) = 1 + hSize l + hSize r
hSize (HNot e) = 1 + hSize e
hSize (HOr l r) = 1 + hSize l + hSize r
hSize (HAnd l r) = 1 + hSize l + hSize r
hSize (HConstruct es) = 1 + sum (map (hSize . snd) es)
hSize (HPipe e1 e2 _) = hSize e1 + hSize e2
hSize (HMap e _) = 1 + hSize e
hSize (HConcat e1 e2) = 1 + hSize e1 + hSize e2
hSize HToList = 1
hSize HFlatten = 1
hSize (Hole _) = 1

data SynthRes
  = SynthRes Program -- the synthetized program
  | ProgramNotFound -- search exhausted all possibilities and didn't found a matching program
  | SynthTimeout -- search spent more than time limit
  deriving (Show, Eq)

-- | timeLimit in microseconds (10^-6)
runSynth :: Int -> [JsonExample] -> IO SynthRes
runSynth timeLimit examples =
  fromMaybe SynthTimeout <$> timeout timeLimit (evaluate go)
  where
    go = maybe ProgramNotFound SynthRes (indGenSynth examples)

indGenSynth :: [JsonExample] -> Maybe Program
indGenSynth examples =
  -- synthesize a program of the form:
  -- \x . e
  -- x : t1, e : t2
  msum $ indGenSearch t1 examples hypotheses
  where
    (t1, t2) = inferVTexamples examples
    hypotheses = inductiveGen t1 t2

-- | for now, the context is a single value (the current argument in scope) but that can be
-- extended eventually
type Context = ValTy

indGenSearch :: Context -> [JsonExample] -> [HExpr] -> [Maybe Program]
indGenSearch t1 examples hs =
  -- split closed and open hypotheses
  -- for every closed hypothesis, check if any is consistent, if it is return that one
  -- else, for each open hypotheses generate more closed/open hypotheses
  -- and recursively search them
  let (closedhs, openhs) = partition isClosed hs
   in case find (consistent examples . hExprToExpr) closedhs of
        Just hConsistent -> [Just . Program . hExprToExpr $ hConsistent]
        Nothing -> case openhs of
          [] -> [Nothing]
          openhs' -> programs
            where
              expandedHypotheses :: [HExpr]
              expandedHypotheses = concatMap (expand t1) openhs'
              programs :: [Maybe Program]
              programs = indGenSearch t1 examples expandedHypotheses

-- given an open hypothesis, that is a node with holes in its leafs
-- returns all the possible fillings of that node.
-- if the toplevel node doesn't have holes, then it recursive traverse it
-- and fills them.
expand :: Context -> HExpr -> [HExpr]
expand t1 hole =
  case hole of
    HConstruct exps
      | anyHoles exps ->
        let kys = map fst exps
            -- quadratic on the number of generated hypotheses by inductiveGen
            allCombinations = mapM (go . snd) exps
            go (Hole (TVal t)) = inductiveGen t1 t
            go x = [x]
         in map (HConstruct . zip kys) allCombinations
    HConstruct exps ->
      let modifiedExps :: [(T.Text, [HExpr])]
          modifiedExps = map (second (expand t1)) exps
          go :: (T.Text, [HExpr]) -> [(T.Text, HExpr)]
          go (k, hes) = hes >>= (\he -> pure (k, he))
       in map HConstruct $ mapM go modifiedExps
    HPipe (Hole (TVal th1)) (Hole (_ `TArrow` TVal t)) _ -> do
      exp1 <- inductiveGen t1 th1
      exp2 <- inductiveGen th1 t
      return $ HPipe exp1 exp2 th1
    HPipe e1 e2 interT -> do
      ex1 <- expand t1 e1
      ex2 <- expand interT e2
      return $ HPipe ex1 ex2 interT
    HMap (Hole (TVal a `TArrow` TVal b)) t -> do
      e <- inductiveGen a b
      return $ HMap e t
    HMap e t ->
      map (`HMap` t) (expand t e)
    HConcat (Hole (TVal lt)) (Hole (TVal rt)) -> do
      el <- inductiveGen t1 lt
      er <- inductiveGen t1 rt
      return $ HConcat el er
    HConcat l r -> do
      el <- expand t1 l
      er <- expand t1 r
      return $ HConcat el er
    HToList -> pure HToList
    HFlatten -> pure HFlatten
    g@(HGet _) -> pure g

    HEqual (Hole (TVal lt)) (Hole (TVal rt))-> do
      el <- inductiveGen t1 lt
      er <- inductiveGen t1 rt
      return $ HEqual el er
    HEqual l r -> do
      el <- expand t1 l
      er <- expand t1 r
      return $ HEqual el er

    HNot (Hole (TVal et)) -> do
      e' <- inductiveGen t1 et
      return $ HNot e'
    HNot e -> do
      e' <- expand t1 e
      return $ HNot e'

    HAnd (Hole (TVal lt)) (Hole (TVal rt))-> do
      el <- inductiveGen t1 lt
      er <- inductiveGen t1 rt
      return $ HAnd el er
    HAnd l r -> do
      el <- expand t1 l
      er <- expand t1 r
      return $ HAnd el er

    HOr (Hole (TVal lt)) (Hole (TVal rt))-> do
      el <- inductiveGen t1 lt
      er <- inductiveGen t1 rt
      return $ HOr el er
    HOr l r -> do
      el <- expand t1 l
      er <- expand t1 r
      return $ HOr el er

    Hole (TVal t) -> inductiveGen t1 t
    h -> error $ T.unpack (prettyHExpr h) <> " " <> show h
  where
    anyHoles :: [(T.Text, HExpr)] -> Bool
    anyHoles = any (isHole . snd)

-- | from an arrow type, return a stream of hypothesis compatible with those types
-- | given a context and a type, return a stream of hypotheses compatible with
-- that type, i.e. hypotheses that can 'fill' such hole.
inductiveGen :: Context -> ValTy -> [HExpr]
inductiveGen ctx t =
  getHs ++ mapHs ++ toListHs ++ flattenHs ++ concatHs ++ constructHs ++ pipeHs ++ equalHs
  ++ notHs ++ andHs ++ orHs
  where
    notHs =
      case t of
        TBool -> [HNot . Hole $ TVal TBool]
        _ -> []
    andHs =
      case t of
        TBool -> [HAnd (Hole $ TVal TBool) (Hole $ TVal TBool)]
        _ -> []
    orHs =
      case t of
        TBool -> [HOr (Hole $ TVal TBool) (Hole $ TVal TBool)]
        _ -> []
    -- equal hypotheses, if the result type is a bool, then the hypotheses is
    -- an equality between the ctx and the hole (the type must be the same)
    equalHs =
      case t of
        TBool ->
          let types = case ctx of
                TObject o -> nub $ M.elems o
                _ -> []
          in do
            ta <- types
            return $ HEqual (Hole $ TVal ta) (Hole $ TVal ta)
        _ -> []
    -- get hypotheses, note how this generates hypotheses without holes
    getHs =
      case ctx of
        TObject o ->
          -- from all the fields of cxt, filter the ones that are equal to t
          -- and return their gets
          map (HGet . fst) $ filter ((t ==) . snd) $ M.toList o
        _ -> []
    -- A construct hypothesis
    -- must satisfy every key-type pair from type t.
    constructHs =
      case t of
        TObject o ->
          [HConstruct $ map (second $ Hole . TVal) $ M.toList o]
        _ -> []
    pipeHs =
      -- if the results is t, then it can be produced
      -- by any pipe expression of the form
      -- a | (a -> t)
      -- a's is evaluated against the cxt
      let types = case ctx of
            TObject o -> nub $ M.elems o
            _ -> []
       in do
            ta <- types
            return $ HPipe (Hole $ TVal ta) (Hole $ ta `tarrow` t) ta
    mapHs =
      case (ctx, t) of
        (TArray a, TArray b) ->
          [HMap (Hole $ TVal a `TArrow` TVal b) a]
        _ -> []
    -- if the result is an array, then it can be produced by concatenating
    -- two arrays of the same type
    concatHs =
      case t of
        TArray _ ->
          [HConcat (Hole $ TVal t) (Hole $ TVal t)]
        _ -> []
    -- if t is an array [a] and ctx is a, then suggest toList
    -- if t is an array [a], suggest a pipe of the form a | toList
    toListHs =
      ( case (ctx, t) of
          (a, TArray b) | a == b -> [HToList]
          _ -> []
      )
        ++ ( case t of
               TArray b -> [HPipe (Hole $ TVal b) HToList b]
               _ -> []
           )
    -- if t is an array, it can be produced by flattening
    -- an array of ts
    flattenHs =
      ( case (ctx, t) of
          (TArray a, b) | a == b -> [HFlatten]
          _ -> []
      )
        ++ ( case t of
               (TArray _) ->
                 let t' = TArray t
                  in [HPipe (Hole $ TVal t') HFlatten t']
               _ -> []
           )
