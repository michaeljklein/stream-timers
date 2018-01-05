{-# LANGUAGE TemplateHaskell #-}

module Test.Timers ( prop_nestPlus
                   , prop_nestMPlus
                   , prop_nestByMPlus
                   , testTimers
                   ) where

import Timers
import Control.Monad
import Test.QuickCheck
import Data.Maybe


prop_nestPlus :: Int -> Bool
prop_nestPlus = liftM3 (\x y z -> (x == y) || (z < 0)) (nest (+1) 0) abs id

prop_nestMPlus :: Int -> Bool
prop_nestMPlus = fromJust . liftM3 (\x y z -> liftM2 (||) (liftM2 (==) x y) ((< 0) <$> z)) (nestM (return . (+1)) 0) (return . abs) return

prop_nestByMPlus :: [Int] -> Int -> Bool
prop_nestByMPlus p = and . liftM3 (\x y z -> liftM2 (||) (liftM2 (==) x y) ((< 0) <$> z)) (nestByM (return . (+1)) 0 . flip asTypeOf p . return . abs) (return . abs) return


return []
testTimers :: IO Bool
-- testTimers = $verboseCheckAll
testTimers = $quickCheckAll

