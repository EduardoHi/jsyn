
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jsyn where


import           GHC.Generics


import qualified Data.Aeson as A
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Either
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Scientific
import           Data.Semigroup
import qualified Data.Text as T
import           Data.Text.Encoding as E
import qualified Data.Vector as V

\end{code}

Since jsyn is a tool for Programming by Example,
we need to represent an example. An JsonExample is a pair of
input and output json values. This datatype can be encoded
and decoded from json.
The ToJSON and FromJSON instances are just how the
Aeson documentation suggests.
\begin{code}
data JsonExample = JsonExample {
  input :: A.Value
  , output :: A.Value
  } deriving (Generic, Show)

instance A.ToJSON JsonExample where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON JsonExample

decodeJsonExamples :: C.ByteString -> Either String [JsonExample]
decodeJsonExamples content =
  A.eitherDecode content :: Either String [JsonExample]

\end{code}

From what I've learned following 
Programming Languages Foundations: https://softwarefoundations.cis.upenn.edu/
A definition of a language should have:
- Terms and Values
- Types

Operational semantics:
- Reductions

Typing Relation
- Progress, Preservation
- inference rules
- terms to types
- subtyping

** Values

\begin{code}

type Object = A.Object
type Value = A.Value

isString :: Value -> Bool
isString (A.String _) = True
isString _ = False

-- TODO: Boolean blindness in isError, isString, Functions :(
-- it would be better to push that to the type level but idk how

fromString :: Value -> T.Text
fromString (A.String s) = s
fromString v = error $ "value is not a string" ++ show v

\end{code}

** Types

A Type represents a subset of possible values, it helps to think about them as
abstracting information, and thinking about sets instead of particular values.

An important thing to notice is that in this typing scheme we assume that
the Arrays are homogenous and they contain the same type in every position,
but this is not the case either the json spec, nor in real life jsons.
This is a place for improvement for the project, and has room for experimentation.

Another important thing to note is that the type of an object is defined
by the name of the keys and the type of each value.

\begin{code}

data Ty
  = TArrow Ty Ty  -- Ty -> Ty
  | TVal ValTy
  deriving (Eq, Show, Read, Ord)

-- TODO: We can create a more sophisticated type system with TRecord and TList
-- that are special cases of when the Object is not being used as a map, and when
-- an array has the same type everywhere
data ValTy
  = TValue                           -- Supertype of the rest
  | TObject (M.HashMap T.Text ValTy) -- {k1: ValTy1, k2 ValTy2, ...} 
  | TArray ValTy                     -- intersection of: [ValTy1, ValTy2, ...]
  | TString                          -- "a string"
  | TNumber                          -- "23"
  | TBool                            -- true
  | TNull                            -- null
  deriving (Eq, Show, Read, Ord)

\end{code}

The separation between Ty and ValTy is mainly to avoid Arrows inside objects and arrays.
Ty represents all possible values in the DSL, and ValTy represents only the values
that are valid JSON. Our final program must receive and return a ValTy, but intermediate
steps might have Tys.

** Subtyping

The Subtyping relation is reflexive and transitive.
It also provides the subsumption rule so that the rules can "forget information"

** Type intersection

Arrays pose an interesting question since json permits heterogeneous arrays.
For example:
What should be the type of ["abc", true, 2.3] ?
What about [{age: 22, name: "Jim"}, {age: 21, name: "Pam", gpa: 4.2}] ?

Also, since the program synthetizer receives a list of input/output examples,
we want to infer what's the type over all inputs and outputs.

Because of this and js code semantics we define the intersection of two objects
as the keys they share and the intersection between their values. For arrays, is the intersection
of their values. For the rest of possible value types, the intersection with themselves is
reflexive and with the rest is the ValTy TValue.

\begin{code}

intersect :: ValTy -> ValTy -> ValTy
intersect a b =
  case (a,b) of
    (TObject ta, TObject tb) -> TObject $ M.intersectionWith intersect ta tb
    (TArray ta, TArray tb)   -> TArray $ ta `intersect` tb
    (TString, TString)       -> TString
    (TNumber, TNumber)       -> TNumber
    (TBool, TBool)           -> TBool
    (TNull, TNull)           -> TNull
    _ -> TValue

\end{code}

intersect can also be thought as the greatest common ancestor in the subtyping tree.

The type of a value naturally follows from it's structure:
\begin{code}

inferVT :: Value -> ValTy
inferVT x =
  case x of
    A.Object o -> TObject $ M.map inferVT o
    A.Array v  -> TArray $ case V.length v of
                            0 -> TValue
                            1 -> inferVT $ V.head v
                            _ -> V.foldl1' intersect $ V.map inferVT v
    A.String _ -> TString
    A.Number _ -> TNumber
    A.Bool _   -> TBool
    A.Null     -> TNull


\end{code}


** DSL 

This DSL is inspired in jq.

but for simplicity I won't add streams, this simplifies the DSL, the type system, inference
synthesis and the output programs.

Our main terms are Filters and Values.

A filter is a function from json values to either a value or a stream,
filters are parametrized on the type of streams they might produce
but not in the type they receive (they always receive values)
this is in part inspired by the robustness principle:
"Be conservative in what you send, be liberal in what you accept"

In the implementation, filters are a datatype, so that it can be manipulated
as data and also be executed with it's corresponding haskell functions.
\begin{code}

data Expr
  = Const Value
  -- /1 arity functions
  | Id
  | Keys
  | Elements
  -- /2 arity functions
  | Get Expr
  | Construct [(Expr, Expr)]
  -- /3 arity functions
  | Union Expr Expr
  | Pipe Expr Expr
  deriving (Show)

fromConstString :: Expr -> T.Text
fromConstString (Const (A.String s)) = s
fromConstString _ = error "not a Const String"

isConstString :: Expr -> Bool
isConstString (Const (A.String _)) = True
isConstString _ = False

\end{code}

** Evaluation

'eval' interprets a filter against a value and returns a value
this can be seen as the "Haskell" interpretation of the DSL
to be used during the search to find the correct programs.
This can also be seen as the spec of the semantics of the DSL,
since I'm not going to formalize it with math.

\begin{code}

type EvalRes = Either String Value

eval :: Expr -> Value -> EvalRes
eval x val = case x of
  -- partially evaluated
  Id           -> pure val
  Const v      -> pure v
  -- partially evaluated
  Get f        -> get val f 
  Construct fs -> construct val fs
  Pipe f g     -> pipe val f g
  -- partially evaluated
  Keys         -> keys val
  -- partially evaluated
  Elements     -> elements val
  Union f g    -> union val f g

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
  (eval f val) >>= lhs
  where
    lhs :: Value -> EvalRes
    lhs l =
      case l of
        A.String v -> getVal v
        _ -> Left "Can't use a non-string as key"
    getVal :: T.Text -> EvalRes
    getVal v = case val of
                 A.Object o -> Right (o M.! v)
                 _ -> Left $ "value: " ++ show val ++ "is not an object" 


construct :: Value -> [(Expr, Expr)] -> EvalRes
construct val fs =
  let kys = map (flip eval val . fst) fs
      vls = map (flip eval val . snd) fs
  in
    construct' kys vls
  where construct' ks vs =
          case (partitionEithers ks, partitionEithers vs) of
            (([],rks), ([],rvs)) -> build rks rvs
            ((lks,_), (lvs,_)) -> Left $ (unlines lks) ++ "\n" ++ (unlines lvs)
        build ks vs =
          if all isString ks
          then Right . A.Object . M.fromList $ zip (map fromString ks) vs
          else Left $ " keys have a value that is not a string: " ++ (show $ head $ takeWhile isString ks)

pipe :: Value -> Expr -> Expr -> EvalRes
pipe v f g =
  eval f =<< eval g v

union :: Value -> Expr -> Expr -> EvalRes
union val f g = do
  l <- eval f val
  r <- eval g val
  case (l,r) of
    (A.Object o1, A.Object o2) -> Right $ A.Object (o1 `M.union` o2)
    (_, A.Object _)            -> Left "Left hand side of union is not an Object"
    (A.Object _,_)             -> Left "Right hand side of union is not an Object"
    (_,_)                      -> Left "union is not with objects"

\end{code}

A Program is what we finally want to have, a function wrapping the filter and returning it:
```js
function program(obj) {
  return filter(obj)
}
```

\begin{code}

newtype Program = Program { programBody :: Expr }

toJS :: Program -> T.Text
toJS Program{programBody = body} = T.unlines
                                   [ "function program(obj) {"
                                   , "return " <> toJSInline "obj" body
                                   , "}"
                                   ]
\end{code}

toJSInline converts Exprs to Javascript code

Since the Exprs have an implicit arg, when we print we need to "thread"
that arg down the tree, and up in the assignments. in the case of the
inlined, the argument is always the same.

\begin{code}

toJSInline :: T.Text -> Expr -> T.Text
toJSInline s x =
  case x of
    (Const v) -> E.decodeUtf8 . C.toStrict $ A.encode v
    Id -> s
    Keys -> "Object.keys(" <> s <> ")"
    Elements -> "Object.values(" <> s <> ")"

    (Get f) -> if isConstString f && (isValidAsKey $ fromConstString f)
               then s <> "." <> fromConstString f
               else s <> "[" <> f' <> "]"
      where f' = toJSInline s f
    (Construct fs) ->
      "{"  <> inlinePairs <> "}"
      where inlinePairs = T.intercalate ", " $ map go fs
            go (k,v) = treatKey k <> ":" <> toJSInline s v
            treatKey kstr =
              if isConstString kstr && (isValidAsKey $ fromConstString kstr)
              then toJSInline s kstr
              else "[" <> toJSInline s kstr <> "]"
    (Union f g) -> "Object.assign(" <> f' <> ", " <> g' <> ")"
      where f' = toJSInline s f
            g' = toJSInline s g
    (Pipe f g) -> f' <> "(" <> g' <> ")"
      where f' = toJSInline s f
            g' = toJSInline s g

-- A valid string for a key does not have spaces or double quotes
isValidAsKey st = not $ T.any (\x -> x == ' ' || x == '"') st

\end{code}
