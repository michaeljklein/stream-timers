module Main where

import Lib
import Test.Lib
import Test.Timers
import Criterion.Main
import Test.QuickCheck
import Control.Monad
import Foreign.C.Types


main :: IO ()
main = do
  putStrLn "Test.Timers:"
  timersSuccess <- testTimers
  unless timersSuccess $ error "Test.Timers failed, aborting test"

  putStrLn "Benchmarking:"
  defaultMain [
    bgroup "getRuns" [
      bench "getRuns  == getRunsA" $ nfIO (benchGetRuns getRuns  getRunsA),
      bench "getRuns  == getRunsB" $ nfIO (benchGetRuns getRuns  getRunsB),
      bench "getRunsA == getRunsB" $ nfIO (benchGetRuns getRunsA getRunsB)
    ],
    bgroup "getRuns'" [
     bench "getRuns'  == getRuns'A" $ nfIO (benchGetRuns' getRuns'  getRuns'A),
     bench "getRuns'  == getRuns'B" $ nfIO (benchGetRuns' getRuns'  getRuns'B),
     bench "getRuns'A == getRuns'B" $ nfIO (benchGetRuns' getRuns'A getRuns'B)
    ]
    ]

isSuccess :: Result -> Bool
isSuccess (Success _ _ _) = True
isSuccess  _              = False

benchGetRuns :: ([ULL] -> [ULL]) -> ([ULL] -> [ULL]) -> IO Bool
benchGetRuns x y = fmap isSuccess . quickCheckWithResult (stdArgs { chatty = False }) $ liftM2 (==) (x . fmap CULLong) (y . fmap CULLong)

benchGetRuns' :: (ULL -> [ULL] -> [ULL]) -> (ULL -> [ULL] -> [ULL]) -> IO Bool
benchGetRuns' x y = fmap isSuccess . quickCheckWithResult (stdArgs { chatty = False }) $ \z -> liftM2 (==) (x (CULLong z) . fmap CULLong) (y (CULLong z) . fmap CULLong)


