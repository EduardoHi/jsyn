-- |
module Main where

import Criterion.Main
import Criterion.Types (Config (..))
import Jsyn

fib :: Integer -> Integer
fib m
  | m < 0 = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n -1) + go (n -2)

myConfig :: Config
myConfig =
  defaultConfig
    { -- where to write the html results report
      reportFile = Just "jsyn-bench.html",
      -- time limit for each benchmark, in seconds
      timeLimit = 30.0
    }

setupEnv1 = readJsonExamples "tests/test1.json"

setupEnv2 = readJsonExamples "tests/test2.json"

setupEnv3 = readJsonExamples "tests/test3.json"

setupEnv4 = readJsonExamples "tests/test4.json"

setupEnv5 = readJsonExamples "tests/test5.json"

setupEnv6 = readJsonExamples "tests/test6.json"

setupEnv7 = readJsonExamples "tests/test7.json"

setupEnv8 = readJsonExamples "tests/test8.json"

setupEnv9 = readJsonExamples "tests/test9.json"

setupEnv10 = readJsonExamples "tests/test10.json"

setupEnv11 = readJsonExamples "tests/test11.json"

setupEnv12 = readJsonExamples "tests/test12.json"

setupEnv13 = readJsonExamples "tests/test13.json"

-- setupEnv14 = readJsonExamples "tests/test14.json"

setupEnv15 = readJsonExamples "tests/test15.json"

-- setupEnv16 = readJsonExamples "tests/test16.json"

main :: IO ()
main =
  defaultMainWith
    myConfig
    [ env setupEnv1 $ \exs -> bench "test1" $ whnf indGenSynth exs,
      env setupEnv2 $ \exs -> bench "test2" $ whnf indGenSynth exs,
      env setupEnv3 $ \exs -> bench "test3" $ whnf indGenSynth exs,
      env setupEnv4 $ \exs -> bench "test4" $ whnf indGenSynth exs,
      env setupEnv5 $ \exs -> bench "test5" $ whnf indGenSynth exs,
      env setupEnv6 $ \exs -> bench "test6" $ whnf indGenSynth exs,
      env setupEnv7 $ \exs -> bench "test7" $ whnf indGenSynth exs,
      env setupEnv8 $ \exs -> bench "test8" $ whnf indGenSynth exs,
      env setupEnv9 $ \exs -> bench "test9" $ whnf indGenSynth exs,
      env setupEnv10 $ \exs -> bench "test10" $ whnf indGenSynth exs,
      env setupEnv11 $ \exs -> bench "test11" $ whnf indGenSynth exs,
      env setupEnv12 $ \exs -> bench "test12" $ whnf indGenSynth exs,
      env setupEnv13 $ \exs -> bench "test13" $ whnf indGenSynth exs,
      -- env setupEnv14 $ \exs -> bench "test14" $ whnf indGenSynth exs,
      env setupEnv15 $ \exs -> bench "test15" $ whnf indGenSynth exs
      -- env setupEnv16 $ \exs -> bench "test16" $ whnf indGenSynth exs
    ]
