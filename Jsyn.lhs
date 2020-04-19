
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jsyn where


import GHC.Generics


import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Scientific
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

type Object = M.HashMap T.Text Value

data Value
  = Object !Object
  | Array ![Value] -- TODO: Vector might be better for indexing?
  | String !T.Text
  | Number !Int -- TODO: change Integer to a sane number thing lol
  | Bool !Bool
  | Null
  | Error String -- our addition to make no-ops explicit,
  -- in practice we should not construct invalid asts in our dsl
  deriving (Eq, Show) 

isError :: Value -> Bool
isError (Error _) = True
isError _ = False

isString :: Value -> Bool
isString (String _) = True
isString _ = False

-- TODO: Boolean blindness in isError, isString, Functions :(
-- it would be better to push that to the type level but idk how

valueToJsonVal :: Value -> A.Value
valueToJsonVal x =
  case x of
    (Object o) -> A.Object $ M.map valueToJsonVal o
    (Array l)  -> A.Array . V.fromList $ map valueToJsonVal l
    (String t) -> A.String t
    (Number n) -> A.Number (read $ show n :: Scientific)
    (Bool b)   -> A.Bool b
    Null       -> A.Null

jsonValToValue :: A.Value -> Value
jsonValToValue x =
  case x of
    (A.Object o) -> Object $ M.map jsonValToValue o
    (A.Array v)  -> Array . V.toList $ V.map jsonValToValue v
    (A.String t) -> String t
    (A.Number n) -> maybe (Error "Unsupported Floats for now") Number (toBoundedInteger n)
    (A.Bool b)   -> Bool b
    A.Null       -> Null

fromString :: Value -> T.Text
fromString (String s) = s
fromString v = T.pack $ "NOT A STRING: " ++ show v

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

Our main terms are Streams and Filters.


A Stream for now is simply a Haskell List.
the most common stream, is a stream of json values

A filter is a function from json values to either a value or a stream,
filters are parametrized on the type of streams they might produce
but not in the type they receive (they always receive values)
this is in part inspired by the robustness principle:
"Be conservative in what you send, be liberal in what you accept"

TODO: Stream can be better represented as a NonEmpty List instead of the Either value
and that simplifies things

TODO: Filters should be datatypes interpreted as functions, but we need a value
representation higher than haskell functions, to eventually synthetize those to js for example.
TFilter is the datatype that represents a Filter will replace the unused Filter type
\begin{code}

type Stream a = [a]

type ValueStream = Stream Value

type Filter out = Value -> Either Value (Stream out)

data TFilter
  = Id
  | Const Value
  | Get TFilter
  | Construct [(TFilter, TFilter)]
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
  Id -> val
  Const v -> v
  Get f -> case val of
             Object o -> get (eval f val) o
             _ -> Error $ "Get of value :" ++ show val ++ " that is not an object"
  Construct fs -> construct val fs
  Pipe f g -> pipe val f g

get :: Value -> Object -> Value
get (String t) obj =
  fromMaybe
  (Error $ "key: " ++ T.unpack t ++ "not found")
  (t `M.lookup` obj)
get notstr _ = Error $ "value: " ++ show notstr ++ "in get is not a string"

construct :: Value -> [(TFilter, TFilter)] -> Value
construct val fs =
  let kys = map (flip eval val . fst) fs
      vls = map (flip eval val . snd) fs
  in
    if all isString kys
    then Object . M.fromList $ zip (map fromString kys) vls
    else Error $ "Some filters in the keys returned other thing other than string: " ++ show kys

pipe :: Value -> TFilter -> TFilter -> Value
pipe v f g =
  eval f $ eval g v

\end{code}
