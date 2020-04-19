{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics


import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

-- an Example is a pair of input and output json values
data Example = Example {
  input :: A.Value
  , output :: A.Value
  } deriving (Generic, Show)

instance A.ToJSON Example where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Example
    -- No need to provide a parseJSON implementation.

-- From what I've learned, a language should have:
-- values
-- terms
-- types
--
-- Then, you define operational semantics, and typing relations

-- Values --------------------------------------------------------------------

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


-- TODO: Boolean blindness in isError, isString, Functions :(
-- it would be better to push that to the type level but idk how

fromString :: Value -> T.Text
fromString (String s) = s
fromString v = T.pack $ "NOT A STRING: " ++ show v

data Ty
  = TObject (M.HashMap String Ty)
  | TArray Ty
  | TString
  | TNumber
  | TBool
  | TNull
  deriving (Eq, Show, Read)


-- DSL ----------------------------------------------------------------

-- It might change eventually, but a Stream for now is simply a Haskell list
type Stream a = [a]

-- the most common stream, is a stream of json values
type ValueStream = Stream Value

-- a filter is a function from json values to either a value or a stream,
-- filters are parametrized on the type of streams they might produce
-- but not in the type they receive (they always receive values)
type Filter out = Value -> Either Value (Stream out)

-- TODO: Stream can be better represented as a NonEmpty List instead of the Either value
-- and that simplifies things

-- TODO: Filters should be datatypes interpreted as functions, but we need a value
-- representation higher than haskell functions, to eventually synthetize those to js for example.

-- TFilter is the datatype that represents a Filter 
data TFilter
  = Id
  | Const Value
  | Get TFilter
  | Construct [(TFilter, TFilter)]
  | Pipe TFilter TFilter
  deriving (Show)

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


-- End of DSL --------------------------------------------------------------------


process :: C.ByteString -> IO ()
process content = 
  case A.eitherDecode content :: Either String [Example] of
         Left s -> putStrLn s
         Right es -> do
           C.putStrLn (A.encode (map (f . input) es))
           C.putStrLn (A.encode (map output es))
  where f e =
          valueToJsonVal $ eval f1 $ jsonValToValue e

f1 :: TFilter
f1 = Construct
  [ (sf, Get sf)
  , (sd, Get sd)
  ]
  where sf = Const (String "foo")
        sd = Const (String "data")


pro_ex1 :: IO ()
pro_ex1 = process "[{\"input\":1,\"output\":2}]"

pro_ex2 :: IO ()
pro_ex2 = process "[{\"input\":1,\"output\":2}, { \"input\":3, \"output\":4 }]"



main :: IO ()
main = do
  content <- C.getContents
  process content
