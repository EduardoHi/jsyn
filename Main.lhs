
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Jsyn

main :: IO ()
main = do
  ex <- readJsonExamples "tests/test1.json"
  case indGenSynth ex of
    Nothing -> putStrLn "Cannot synthetize program"
    Just prg -> putStrLn . T.unpack $ toJS prg

\end{code}
