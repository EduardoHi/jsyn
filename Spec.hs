{-# LANGUAGE OverloadedStrings #-}

-- |
module Main where

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either
import qualified Data.HashMap.Strict as M
import Data.Semigroup
import qualified Data.Text as T
import Jsyn
import Test.Hspec
import Text.Printf

readInputOutputPairs :: String -> IO [(A.Value, A.Value)]
readInputOutputPairs filename = do
  content <- C.readFile filename
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> pure $ map (\v -> (input v, output v)) v

readJsonExamples :: String -> IO [JsonExample]
readJsonExamples filename = do
  content <- C.readFile filename
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> return v

-- | cstring little helper to build Constant Strings in the DSL
cstring = Const . A.String

filter_ex1 :: Expr
filter_ex1 =
  Construct
    [ (sf, Get sf),
      (sd, Get sd)
    ]
  where
    sf = cstring "foo"
    sd = cstring "data"

filter1 :: Expr
filter1 = Id

filter2 :: Expr
filter2 =
  Construct
    [ (Get sc, sc),
      (Get ss, ss)
    ]
  where
    sc = cstring "Chicago"
    ss = cstring "Seattle"

filter3 :: Expr
filter3 =
  Construct
    [ ( Get $ cstring "key",
        Construct
          [ (cstring "host", Get $ cstring "host"),
            (cstring "name", Get $ cstring "name")
          ]
      )
    ]

-- equivalent to this jq filter: {key: (keys | .[]) } + .[]
filter4 :: Expr
filter4 =
  Union
    (Construct [(cstring "key", Keys)])
    Elements

filter5 :: Expr
filter5 = Get $ cstring "age"

-- TODO: This would fail with objects that share a key but have different fields
-- a more succint way is for the DSL to have a Del operation
filter6 :: Expr
filter6 =
  Construct
    [ (cstring "age", Get $ cstring "age"),
      (cstring "gpa", Get $ cstring "gpa")
    ]

types1 :: [(ValTy, ValTy)]
types1 =
  [ ( TObject (M.fromList [("age", TNumber), ("name", TString)]),
      TObject (M.fromList [("age", TNumber), ("name", TString)])
    )
  ]

types2 :: [(ValTy, ValTy)]
types2 =
  [ ( TObject (M.fromList [("Chicago", TString), ("Seattle", TString)]),
      TObject (M.fromList [("Los Angeles", TString), ("New York", TString)])
    )
  ]

types3 :: [(ValTy, ValTy)]
types3 =
  [ ( TObject (M.fromList [("key", TString), ("name", TString), ("host", TString)]),
      TObject
        ( M.fromList
            [ ( "key1",
                TObject (M.fromList [("name", TString), ("host", TString)])
              )
            ]
        )
    ),
    ( TObject (M.fromList [("key", TString), ("name", TString), ("host", TString)]),
      TObject
        ( M.fromList
            [ ( "key2",
                TObject (M.fromList [("name", TString), ("host", TString)])
              )
            ]
        )
    )
  ]

types4 :: [(ValTy, ValTy)]
types4 =
  [ ( TObject
        ( M.fromList
            [ ( "key1",
                TObject (M.fromList [("name", TString), ("host", TString)])
              )
            ]
        ),
      TObject (M.fromList [("key", TString), ("name", TString), ("host", TString)])
    ),
    ( TObject (M.fromList [("key2", TObject (M.fromList [("name", TString), ("host", TString)]))]),
      TObject (M.fromList [("key", TString), ("name", TString), ("host", TString)])
    )
  ]

types5 :: [(ValTy, ValTy)]
types5 =
  [ ( TObject (M.fromList [("age", TNumber), ("name", TString)]),
      TNumber
    )
  ]

types6 :: [(ValTy, ValTy)]
types6 =
  [ ( TObject (M.fromList [("age", TNumber), ("name", TString), ("gpa", TNumber)]),
      TObject (M.fromList [("age", TNumber), ("gpa", TNumber)])
    )
  ]

data TestTask = TestTask
  { taskName :: String,
    taskExpr :: Expr,
    taskIO :: IO [(A.Value, A.Value)],
    expectedTypes :: [(ValTy, ValTy)]
  }

testCases :: [TestTask]
testCases =
  [ TestTask
      { taskName = "identity filter",
        taskExpr = filter1,
        taskIO = readInputOutputPairs "tests/test1.json",
        expectedTypes = types1
      },
    -- Can this be generic enough to not need the specific key names ?
    TestTask
      { taskName = "swap every key for its value",
        taskExpr = filter2,
        taskIO = readInputOutputPairs "tests/test2.json",
        expectedTypes = types2
      },
    TestTask
      { taskName = "nest an object inside a a key from the field 'key'",
        taskExpr = filter3,
        taskIO = readInputOutputPairs "tests/test3.json",
        expectedTypes = types3
      },
    TestTask
      { taskName = "denest an object (inverse operation of filter3)",
        taskExpr = filter4,
        taskIO = readInputOutputPairs "tests/test4.json",
        expectedTypes = types4
      },
    TestTask
      { taskName = "get a single field from an object",
        taskExpr = filter5,
        taskIO = readInputOutputPairs "tests/test5.json",
        expectedTypes = types5
      },
    TestTask
      { taskName = "get all but a single field from an object",
        taskExpr = filter6,
        taskIO = readInputOutputPairs "tests/test6.json",
        expectedTypes = types6
      }
  ]

fromEval :: EvalRes -> A.Value
fromEval (Right v) = v
fromEval (Left err) = error err

testEval :: String -> Expr -> [(A.Value, A.Value)] -> SpecWith ()
testEval name expr ios =
  describe "eval"
    $ forM_ (zip [1 ..] ios)
    $ \(n, (input, output)) ->
      it ("example #" <> show n <> " evaluates correctly") $
        fromEval (eval expr input) `shouldBe` output

testInferVal :: String -> [(ValTy, ValTy)] -> [(A.Value, A.Value)] -> SpecWith ()
testInferVal name expected ios =
  describe "inference" $
    forM_ (zip3 [1 ..] expected ios) go
  where
    go (n, (expected_i, expected_o), (input, output)) =
      do
        it ("example #" <> show n <> " infers input type correctly") $
          inferVT input `shouldBe` expected_i
        it ("example #" <> show n <> " infers output type correctly") $
          inferVT output `shouldBe` expected_o

main :: IO ()
main = hspec $ do
  let exprs = map taskExpr testCases
  let expected_types = map expectedTypes testCases

  forM_ testCases $ \(TestTask name expr readios expected_types) -> do
    ios <- runIO readios
    describe ("task: " <> name) $ do
      testEval name expr ios
      testInferVal name expected_types ios
