-- | 

module Main where

import Test.Hspec

import           Control.Monad
import           Data.Semigroup
import qualified Data.Text.IO     as Text
import           Text.Printf

-- readExamples :: IO [(Text, Text)]
-- readExamples =
--   mapM asPair =<< Text.readFile "test/plurals.csv"
--   where
--     asPair line =
--       case Text.splitOn "," line of
--         [input, expected] -> pure (input, expected)
--         _ -> fail ("Invalid example line: " <> Text.unpack line)


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
