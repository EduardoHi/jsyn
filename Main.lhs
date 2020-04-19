
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C

import Jsyn

\end{code}

* Examples

\begin{code}

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


process :: C.ByteString -> IO ()
process content = 
  case A.eitherDecode content :: Either String [Example] of
         Left s -> putStrLn s
         Right es -> do
           C.putStrLn (A.encode (map (f . input) es))
           C.putStrLn (A.encode (map output es))
  where f e =
          valueToJsonVal $ eval f1 $ jsonValToValue e

main :: IO ()
main = do
  content <- C.getContents
  process content

\end{code}
