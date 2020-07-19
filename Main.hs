{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Jsyn
import System.Environment


-- readJsonExamples |> runSynth |> toJS

main :: IO ()
main = do
  args <- getArgs
  if length args > 1
    then putStrLn "usage: jsyn <examples file>"
    else do
      ex <- readJsonExamples (head args)
      res <- runSynth (2 * 10^6) ex
      case res of
        SynthTimeout -> putStrLn "synthesis ran out of time"
        ProgramNotFound -> putStrLn "Exhausted all possibilities and didn't find a valid program"
        SynthRes prg -> putStrLn . T.unpack $ toJS prg
