{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Timers ( prop_nestPlus
                   , prop_nestMPlus
                   , prop_nestByMPlus
                   , testTimers
                   ) where

import Timers
import Control.Monad
import Test.QuickCheck


prop_nestPlus :: Int -> Bool
prop_nestPlus = liftM3 (\x y z -> (x == y) || (z < 0)) (nest (+1) 0) abs id

prop_nestMPlus :: forall (m :: * -> *). Monad m => Int -> m Bool
prop_nestMPlus = liftM3 (\x y z -> liftM2 (||) (liftM2 (==) x y) ((< 0) <$> z)) (nestM (return . (+1)) 0) (return . abs) return

prop_nestByMPlus :: forall (m :: * -> *) (t :: * -> *). (Monad m, Monad t, Foldable t) => t Int -> Int -> m Bool
prop_nestByMPlus p = liftM3 (\x y z -> liftM2 (||) (liftM2 (==) x y) ((< 0) <$> z)) (nestByM (return . (+1)) 0 . flip asTypeOf p . return) (return . abs) return


return []
testTimers :: IO Bool
testTimers = $verboseCheckAll
-- testTimers = $quickCheckAll

