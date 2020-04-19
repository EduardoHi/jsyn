{-# LANGUAGE OverloadedStrings #-}
-- | 

module Main where

import Test.Hspec

import Jsyn

import           Control.Monad
import           Data.Semigroup
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import           Text.Printf
import Data.HashMap.Strict (fromList)


readInputValues :: IO [Value]
readInputValues = do
  content <- C.readFile "examples/example1.json"
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> pure $ map (jsonValToValue . input) v

filter1 :: TFilter
filter1 = Construct
          [ (sf, Get sf)
          , (sd, Get sd)
          ]
  where sf = Const (String "foo")
        sd = Const (String "data")

output_values :: [Value]
output_values = [
  Object (fromList
          [("data",Array [Object (fromList [("a",Number 1)]),
                          Object (fromList [("b",Number 2)])]),
           ("foo",String "bar1")]),
  Object (fromList
          [("data",Array [Object (fromList [("a",Number 1)]),
                          Object (fromList [("b",Number 2)])]),
           ("foo",String "bar2")])
  ]

main :: IO ()
main = hspec $ do
  input_values <- runIO readInputValues
  
  forM_  (zip input_values output_values) $ \(input, output) ->
    it "evaluates" $
      eval filter1 input == output
                            
