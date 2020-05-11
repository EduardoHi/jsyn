{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Jsyn
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args > 1
    then putStrLn "usage: jsyn <examples file>"
    else do
      ex <- readJsonExamples (head args)
      case indGenSynth ex of
        Nothing -> putStrLn "Cannot synthetize program"
        Just prg -> putStrLn . T.unpack $ toJS prg
