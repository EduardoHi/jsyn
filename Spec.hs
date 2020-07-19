{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module Main where

import Control.Monad
import qualified Data.Aeson as A


import qualified Data.HashMap.Strict as M

import Jsyn
import Test.Hspec


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

filter7 :: Expr
filter7 =
  Pipe (Get (cstring "hostinfo")) (Get (cstring "host"))

filter9 =
  EMap
    ( Construct
        [ (cstring "host", Pipe (Get $ cstring "hostinfo") (Get $ cstring "host")),
          (cstring "name", Pipe (Get $ cstring "hostinfo") (Get $ cstring "name")),
          (cstring "online", Pipe (Get $ cstring "hostinfo") (Get $ cstring "online")),
          (cstring "id", Get $ cstring "id")
        ]
    )

filter12 =
  Construct
    [ (cstring "name", Get $ cstring "name"),
      (cstring "numbers", LConcat (Get $ cstring "number1") (Get $ cstring "number2"))
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

types7 =
  [ ( TObject
        ( M.fromList
            [ ( "hostinfo",
                TObject
                  (M.fromList [("online", TBool), ("name", TString), ("host", TString)])
              ),
              ("id", TNumber)
            ]
        ),
      TString
    )
  ]

data TestTask = TestTask
  { taskName :: String,
    taskExpr :: Expr,
    taskIO :: IO [JsonExample],
    expectedTypes :: [(ValTy, ValTy)]
  }

testCases :: [TestTask]
testCases =
  [ TestTask
      { taskName = "identity filter",
        taskExpr = filter1,
        taskIO = readJsonExamples "tests/test1.json",
        expectedTypes = types1
      },
    -- Can this be generic enough to not need the specific key names ?
    TestTask
      { taskName = "swap every key for its value",
        taskExpr = filter2,
        taskIO = readJsonExamples "tests/test2.json",
        expectedTypes = types2
      },
    TestTask
      { taskName = "nest an object inside a a key from the field 'key'",
        taskExpr = filter3,
        taskIO = readJsonExamples "tests/test3.json",
        expectedTypes = types3
      },
    TestTask
      { taskName = "denest an object (inverse operation of filter3)",
        taskExpr = filter4,
        taskIO = readJsonExamples "tests/test4.json",
        expectedTypes = types4
      },
    TestTask
      { taskName = "get a single field from an object",
        taskExpr = filter5,
        taskIO = readJsonExamples "tests/test5.json",
        expectedTypes = types5
      },
    TestTask
      { taskName = "get all but a single field from an object",
        taskExpr = filter6,
        taskIO = readJsonExamples "tests/test6.json",
        expectedTypes = types6
      },
    TestTask
      { taskName = "get a value in a nested object",
        taskExpr = filter7,
        taskIO = readJsonExamples "tests/test7.json",
        expectedTypes = types7
      }
  ]

fromEval :: EvalRes -> A.Value
fromEval (Right v) = v
fromEval (Left err) = error err

testEval :: String -> Expr -> [JsonExample] -> SpecWith ()
testEval name expr ios =
  describe "eval"
    $ forM_ (zip [1 ..] ios)
    $ \(n, JsonExample {input, output}) ->
      it ("example #" <> show n <> " evaluates correctly") $
        fromEval (eval expr input) `shouldBe` output

testInferVal :: String -> [(ValTy, ValTy)] -> [JsonExample] -> SpecWith ()
testInferVal name expected ios =
  describe "inference" $
    forM_ (zip3 [1 ..] expected ios) go
  where
    go (n, (expected_i, expected_o), JsonExample {input, output}) =
      do
        it ("example #" <> show n <> " infers input type correctly") $
          inferVT input `shouldBe` expected_i
        it ("example #" <> show n <> " infers output type correctly") $
          inferVT output `shouldBe` expected_o

testTasks :: [TestTask] -> SpecWith ()
testTasks tasks =
  forM_ (zip [1 ..] tasks) $ \(i, TestTask name expr readios expected_types) -> do
    ios <- runIO readios
    describe ("task " <> show i <> ": " <> name) $ do
      testEval name expr ios
      testInferVal name expected_types ios

testSynthetizer :: String -> SpecWith ()
testSynthetizer filename = do
  examples <- runIO $ readJsonExamples filename
  describe ("synthetizes " <> filename) $ do
    res <- runIO $ runSynth (5* 10^6) examples

    it "doesn't time out" $ res `shouldNotBe` SynthTimeout
    it "doesn't fail to find a program" $ res `shouldNotBe` ProgramNotFound

    it "is consistent with examples" $ do
      let (SynthRes Program {programBody = expr}) = res
      expr `shouldSatisfy` consistent examples

main :: IO ()
main =
  hspec $ do
    testTasks testCases

    describe "inductive generalization search" $ do
      -- fails, identity function is missing...
      testSynthetizer "tests/test1.json"
      -- fails, can't infer values used as keys
      testSynthetizer "tests/test2.json"
      -- fails, can't infer value used as key
      testSynthetizer "tests/test3.json"
      -- fails, inverse of test 3
      testSynthetizer "tests/test4.json"

      testSynthetizer "tests/test5.json"
      testSynthetizer "tests/test6.json"
      testSynthetizer "tests/test7.json"
      testSynthetizer "tests/test8.json"
      testSynthetizer "tests/test9.json"
      testSynthetizer "tests/test10.json"
      testSynthetizer "tests/test11.json"
      testSynthetizer "tests/test12.json"
      testSynthetizer "tests/test13.json"
    -- fails, can't handle heterogeneous lists correctly yet
      testSynthetizer "tests/test14.json"

      testSynthetizer "tests/test15.json"
      testSynthetizer "tests/test16.json"
      testSynthetizer "tests/test17.json"
      testSynthetizer "tests/test18.json"

      -- equality on numbers
      testSynthetizer "tests/test20.json"

      -- fails, equality on values
      testSynthetizer "tests/test21.json"

      -- fails, needs id, bool not
      testSynthetizer "tests/test22.json"

      -- bool and
      testSynthetizer "tests/test23.json"

      -- bool or
      testSynthetizer "tests/test24.json"

      -- bool xor
      testSynthetizer "tests/test25.json"
