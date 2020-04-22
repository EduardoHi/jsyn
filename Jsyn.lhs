
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
import qualified Data.Text as T
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


data Val
  = JsonVal A.Value
-- our addition to make no-ops explicit,
-- in practice we should not construct invalid asts in our dsl
  | Error String
  deriving (Eq, Show)

isError :: Val -> Bool
isError (Error _) = True
isError _ = False

-- isString :: Val -> Bool
isString (A.String _) = True
isString _ = False

-- TODO: Boolean blindness in isError, isString, Functions :(
-- it would be better to push that to the type level but idk how

fromString :: Value -> T.Text
fromString (A.String s) = s
fromString v = error $ "value is not a string" ++ show v

\end{code}

** Types

A Type represents a subset of possible values, I visualize it as a way 
of abstracting information, and thinking about sets instead of particular values.

An important thing to notice is that in this typing scheme we assume that
the Arrays are homogenous and they contain the same type in every position,
but this is not the case either the json spec, nor in real life jsons.
This is a place for improvement for the project, and has room for experimentation.

Another important thing to note is that the type of an object is defined
by the name of the keys and the type of each value.

\begin{code}

data Ty
  = TObject (M.HashMap String Ty)
  | TArray Ty
  | TString
  | TNumber
  | TBool
  | TNull
  deriving (Eq, Show, Read)

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

data TFilter
  = Const Value
  -- /1 arity functions
  | Id
  | Keys
  | Elements
  -- /2 arity functions
  | Get TFilter
  | Construct [(TFilter, TFilter)]
  -- /3 arity functions
  | Union TFilter TFilter
  | Pipe TFilter TFilter
  deriving (Show)

\end{code}

** Evaluation

'eval' interprets a filter against a value and returns a value
this can be seen as the "Haskell" interpretation of the DSL
to be used during the search to find the correct programs.
This can also be seen as the spec of the semantics of the DSL,
since I'm not going to formalize it with math.

\begin{code}

eval :: TFilter -> Value -> Value
eval x val = case x of
  Id           -> val
  Const v      -> v
  Get f        -> get val f 
  Construct fs -> construct val fs
  Pipe f g     -> pipe val f g
  Keys         -> keys val
  Elements     -> elements val
  Union f g    -> union val f g

keys :: Value -> Value
keys (A.Object o) =
  A.Array . V.fromList $ map A.String $ M.keys o
keys val = error $ "called keys of value: " ++ show val ++ "that is not an object"

elements :: Value -> Value
elements (A.Object o) = A.Array . V.fromList $ M.elems o
elements val = error $ "called elems of value: " ++ show val ++ "that is not an object"

-- | val : the value from eval
-- | f   : the filter that evaluated returns the key for the object val

-- 1. eval the filter with the current value
-- if it is a single value:
-- 2. if it's
get :: Value -> TFilter -> Value
get val f =
  case eval f val of
    A.String v -> getVal v
    _ -> error "Can't use a non-string as key"
  where
    getVal :: T.Text -> Value
    getVal v = case val of
                 A.Object o -> (o M.! v)
                 _ -> error $ "value: " ++ show val ++ "is not an object" 


construct :: Value -> [(TFilter, TFilter)] -> Value
construct val fs =
  let kys = map (flip eval val . fst) fs
      vls = map (flip eval val . snd) fs
  in
    if all isString kys
    then A.Object . M.fromList $ zip (map fromString kys) vls
    else error $ " keys have a value that is not a string: " ++ (show $ head $ takeWhile isString kys)

pipe :: Value -> TFilter -> TFilter -> Value
pipe v f g =
  eval f $ eval g v

union :: Value -> TFilter -> TFilter -> Value
union val f g =
  case (eval f val, eval g val) of
    (A.Object o1, A.Object o2) -> A.Object (o1 `M.union` o2)
    (_, A.Object _           ) -> error "Left hand side of union is not an Object"
    (A.Object _,_            ) -> error "Right hand side of union is not an Object"

\end{code}
