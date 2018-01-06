module Main where

import Data.Timers.Example
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let arg = if null args
              then error "No arguements passed! Please provide a somewhat large number."
              else read (head args)
  returns <- returnEveryN arg
  print $ returns


