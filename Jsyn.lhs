
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

In the implementation, filters are a datatype, so that it can be manipulated
as data and also be executed with it's corresponding haskell functions.
\begin{code}

type Stream a = [a]

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

type EvalRes = Either Value (Stream Value)

eval :: TFilter -> Value -> EvalRes
eval x val = case x of
  Id           -> Left val
  Const v      -> Left v
  Get f        -> get val f 
  Construct fs -> construct val fs
  Pipe f g     -> pipe val f g
  Keys         -> keys val
  Elements     -> elements val
  Union f g    -> union val f g

keys :: Value -> EvalRes
keys (A.Object o) =
  Right $ map A.String $ M.keys o
keys val = error $ "called keys of value: " ++ show val ++ "that is not an object"

elements :: Value -> EvalRes
elements (A.Object o) = Right $ M.elems o
elements val = error $ "called elems of value: " ++ show val ++ "that is not an object"

-- | val : the value from eval
-- | f   : the filter that evaluated returns the key for the object val

-- 1. eval the filter with the current value
-- if it is a single value:
-- 2. if it's
get :: Value -> TFilter -> EvalRes
get val f =
  either (Left . fv) (Right . map fv) (eval f val)
  where
    fv :: Value -> Value
    fv vak = case vak of
               A.String v -> getVal v
               _ -> error "Can't use a non-string as key"
    getVal :: T.Text -> Value
    getVal v = case val of
                 A.Object o -> (o M.! v)
                 _ -> error $ "value: " ++ show val ++ "is not an object" 


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [ (x,y) | x <- xs, y <- ys ]

-- |
-- >> λ> weird_prod [([1,2],[3,4]), ([5,6], [7,8]), ([9], [10])]
-- >> [[(1,3),(5,7),(9,10)],[(1,3),(5,8),(9,10)],[(1,3),(6,7),(9,10)],[(1,3),(6,8),(9,10)],[(1,4),(5,7),(9,10)],[(1,4),(5,8),(9,10)],[(1,4),(6,7),(9,10)],[(1,4),(6,8),(9,10)],[(2,3),(5,7),(9,10)],[(2,3),(5,8),(9,10)],[(2,3),(6,7),(9,10)],[(2,3),(6,8),(9,10)],[(2,4),(5,7),(9,10)],[(2,4),(5,8),(9,10)],[(2,4),(6,7),(9,10)],[(2,4),(6,8),(9,10)]]
-- weirdProd :: Monad m => [(m a, m b)] -> m [(a, b)]
weirdProd :: [([a], [b])] -> [[(a, b)]]
weirdProd xs = mapM (uncurry cartProd) xs

construct :: Value -> [(TFilter, TFilter)] -> EvalRes
construct val fs =
  let kys = map (flip eval val . fst) fs
      vls = map (flip eval val . snd) fs
      -- kys' and vls' hold lists instead of eithers. left is a singleton list
      kys' = map (either return id) kys
      vls' = map (either return id) vls
      kvs = zip kys' vls'
  in
    Right $ map (\l -> A.Object $ M.fromList $ map (first fromString) l) $ weirdProd kvs

pipe :: Value -> TFilter -> TFilter -> EvalRes
pipe v f g =
  case eval g v of
    Left v' -> eval f v'
    Right vs -> let vs' = map (eval f) vs
                in Right $ concatMap (either pure id) vs'

union :: Value -> TFilter -> TFilter -> EvalRes
union val f g =
  case (eval f val, eval g val) of
    (Left v, Left w    ) -> Left $ union_obj v w
    (Left v, Right ws  ) -> Right $ map (union_obj v) ws
    (Right vs, Left w  ) -> Right $ map (flip union_obj w) vs
    (Right vs, Right ws) -> Right $ concatMap (\v -> map (union_obj v) ws) vs
  where
    union_obj :: Value -> Value -> Value
    union_obj obj1 obj2 = case (obj1,obj2) of
                      (A.Object o1, A.Object o2) -> A.Object (o1 `M.union` o2)
                      (_, A.Object _           ) -> error "Left hand side of union is not an Object"
                      (A.Object _,_            ) -> error "Right hand side of union is not an Object"

\end{code}
