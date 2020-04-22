{-# LANGUAGE OverloadedStrings #-}
-- | 

module Main where

import           Test.Hspec

import           Jsyn

import           Control.Monad
import           Data.Semigroup
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import           Data.HashMap.Strict (fromList)
import qualified Data.Aeson as A
import           Data.Either

import           Text.Printf

readInputOutputPairs :: String -> IO [(A.Value,A.Value)]
readInputOutputPairs filename = do
  content <- C.readFile filename
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> pure $ map (\v -> (input v , output v)) v
  

-- | cstring little helper to build Constant Strings in the DSL
cstring = Const . A.String

filter_ex1 :: TFilter
filter_ex1 = Construct
          [ (sf, Get sf)
          , (sd, Get sd)
          ]
  where sf = cstring "foo"
        sd = cstring "data"

filter1 :: TFilter
filter1 = Id

filter2 :: TFilter
filter2 = Construct
          [ (Get sc, sc)
          , (Get ss, ss)
          ]
  where sc = cstring "Chicago"
        ss = cstring "Seattle"

filter3 :: TFilter
filter3 = Construct
          [ (Get $ cstring "key",
              Construct
              [ (cstring "host", Get $ cstring "host")
              , (cstring "name", Get $ cstring "name")
              ]
            )
          ]

-- TODO: when eval becomes streams, this should be possible !
filter4 :: TFilter
filter4 = Union
          (Construct [(cstring "key", Keys)])
          Elements

filter5 :: TFilter
filter5 = Get $ cstring "age"

-- TODO: This would fail with objects that share a key but have different fields
-- a more succint way is for the DSL to have a Del operation
filter6 :: TFilter
filter6 = Construct
          [ (cstring "age", Get $ cstring "age")
          , (cstring "gpa", Get $ cstring "gpa")
          ]

testCases :: [(String, TFilter, IO [(A.Value, A.Value)])]
testCases =
  [ ("identity filter",
     filter1,
     readInputOutputPairs "tests/test1.json")

  -- Can this be generic enough to not need the specific key names ?
  , ( "swap every key for its value"
    , filter2
    , readInputOutputPairs "tests/test2.json")

  , ( "nest an object inside a a key from the field 'key'"
    , filter3
    , readInputOutputPairs "tests/test3.json")

  , ( "denest an object (inverse operation of filter3)"
    , filter4
    , readInputOutputPairs "tests/test4.json")

  , ( "get a single field from an object"
    , filter5
    , readInputOutputPairs "tests/test5.json")

  , ( "get all but a single field from an object"
    , filter6
    , readInputOutputPairs "tests/test6.json")
    
  ]

testFilter :: String -> TFilter -> [(A.Value, A.Value)] -> SpecWith ()
testFilter name filter ios = do
  describe ("filter: " <> name) $ do
    forM_  (zip [1..] ios) $ \(n, (input, output)) ->
      it ("example #" <> show n <> " evaluates correctly") $ do
      -- TODO: make a specific eval with only single value outputs
      let output' = case eval filter input of
                      Left x -> x
                      Right xs -> head xs
        in output' == output
main :: IO ()
main = hspec $ do

  forM_ testCases $ \(name, f, readios) -> do
    ios <- runIO readios
    testFilter name f ios
