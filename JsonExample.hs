{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonExample
  ( JsonExample (..),
    decodeJsonExamples,
    readJsonExamples
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C

data JsonExample = JsonExample
  { input :: A.Value,
    output :: A.Value
  }
  deriving (Generic, Show, NFData)

instance A.ToJSON JsonExample where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON JsonExample

decodeJsonExamples :: C.ByteString -> Either String [JsonExample]
decodeJsonExamples content =
  A.eitherDecode content :: Either String [JsonExample]

readJsonExamples :: String -> IO [JsonExample]
readJsonExamples filename = do
  content <- C.readFile filename
  case decodeJsonExamples content of
    Left s -> fail $ "Error decoding json: " <> s
    Right v -> return v
