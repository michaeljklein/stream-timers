{-# LANGUAGE RankNTypes #-}

module Data.Timers.Example
    ( Map,
      gapless,
      gapless',
      gaplessEq,
      countGapless,
      countGapless',
      nextGapless,
      onlyGapless,
      toMathematica,
      onlyGaplessMathematica,
      differences,
      differences_,
      prop_differences_,
      getRuns,
      getRunsA,
      getRunsB,
      getRuns',
      getRuns'A,
      getRuns'B,
      addPosition,
      theStream,
      printMap,
      printEveryNOfStream,
      returnEveryN
    ) where


import Data.Bits (Bits, (.&.), unsafeShiftR)
-- import Foreign.C.Types (CULLong)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Timers (foldlOnce, sideEffectEveryN, printEveryN) -- foldrTimes,
import Control.Monad (liftM2)
import Data.Map.Internal.Debug (showTreeWith)
import Data.List (group)


-- | Mystery function... (read the source)
-- {-# SPECIALIZE gapless :: CULLong -> Bool #-}
gapless :: (Bits t, Num t) => t -> Bool
gapless z = if z == 0
               then True
               else if 3 .&. z == 0              -- if mod z 4 == 0
               then gapless  $ unsafeShiftR z 2  -- gapless (div z 2)
               else gapless' $ unsafeShiftR z 2

-- | Helper function to `gapless`
-- {-# SPECIALIZE gapless' :: CULLong -> Bool #-}
gapless' :: (Bits t, Num t) => t -> Bool
gapless' z = if z == 0
                then True
                else if 3 .&. z == 0
                then False
                else gapless' $ unsafeShiftR z 2


-- | Number of `gapless` numbers in @[1..n]@
-- {-# SPECIALIZE countGapless :: Num a => CULLong -> a #-}
countGapless :: (Bits a, Enum a, Num a, Num b) => a -> b
countGapless n = sum [ sum [ 1 | gapless i] | i<-[1..n]]

-- | Number of `gapless'` numbers in @[1..n]@
-- {-# SPECIALIZE countGapless' :: Num a => CULLong -> a #-}
countGapless' :: (Bits a, Enum a, Num a, Num b) => a -> b
countGapless' n = sum [ sum [ 1 | gapless' i] | i<-[1..n]]

-- | Are `gapless` and `gapless'` equal?
-- {-# SPECIALIZE gaplessEq :: CULLong -> Bool #-}
gaplessEq :: (Bits a, Num a) => a -> Bool
gaplessEq = liftM2 (==) gapless gapless'



-- | What it says on the box
-- {-# SPECIALIZE nextGapless :: CULLong -> CULLong #-}
nextGapless :: (Bits a, Num a) => a -> a
nextGapless = until gapless (+1)


-- | One-liner to export gapless to mathematica:
--
-- @
--  concatMap (\x->concat["{",show(head x),",",show(length x),"},"]).group . (\x->zipWith(-)(tail x)x) . onlyGapless $ [1..100]
-- @
--
-- See @only_gapless.md@
-- {-# SPECIALIZE onlyGapless :: [CULLong] -> [CULLong] #-}
onlyGapless :: (Bits t, Num t) => [t] -> [t]
onlyGapless = filter gapless

-- {-# SPECIALIZE onlyGaplessMathematica ::         [CULLong] -> [Char] #-}
onlyGaplessMathematica :: (Bits t, Num t, Show t) => [t] -> String
onlyGaplessMathematica = toMathematica . group . differences . onlyGapless

-- {-# SPECIALIZE toMathematica ::         [[CULLong]] -> [Char] #-}
toMathematica :: (Show a, Foldable t) => t [a] -> String
toMathematica = concatMap (\x->concat["{",show(head x),",",show(length x),"},"])


-- | First differences:
--
-- @
--  [x0, x1, x2, x3..] -> [x1 - x0, x2 - x1, x3 - x2..]
-- @
--
differences :: Num a => [a] -> [a]
differences = zipWith (-) =<< tail

-- | Should be equivalent to `differences`
differences_ :: Num a => [a] -> [a]
differences_ = (\x->zipWith(-)(tail x)x)

prop_differences_ :: [Int] -> Bool
prop_differences_ = liftM2 (==) differences differences_


-- | Warning, this function is incomplete.
-- {-# SPECIALIZE getRuns  ::            [CULLong] -> [CULLong] #-}
getRuns :: (Num a, Num b, Eq a) => [a] -> [b]
getRuns    (1:xs) =     getRuns        xs
getRuns    (_:xs) =     getRuns'  1    xs
getRuns     _     = error "getRuns: [] is undefined"




-- | A "match [] first" version of `getRuns`
-- {-# SPECIALIZE getRunsA  ::            [CULLong] -> [CULLong] #-}
getRunsA :: (Num a, Num b, Eq a) => [a] -> [b]
getRunsA      []    =     []
getRunsA     (1:xs) =     getRunsA        xs
getRunsA    ~(_:xs) =     getRuns'A  1    xs

-- | A "match [] last" version of `getRuns`
-- {-# SPECIALIZE getRunsB  ::            [CULLong] -> [CULLong] #-}
getRunsB :: (Num a, Num b, Eq a) => [a] -> [b]
getRunsB    (1:xs) =     getRunsB        xs
getRunsB    (_:xs) =     getRuns'B  1    xs
getRunsB     _     =     []


-- | Warning, this function is incomplete.
-- {-# SPECIALIZE getRuns' :: CULLong -> [CULLong] -> [CULLong] #-}
getRuns' :: (Num a, Num b, Eq a) => b -> [a] -> [b]
getRuns' n (1:xs) = n : getRuns        xs
getRuns' n (_:xs) =     getRuns' (n+1) xs
getRuns' _  _     = error "getRuns' _ [] is undefined"

-- | A "match [] first" version of `getRuns'`
-- {-# SPECIALIZE getRuns'A :: CULLong -> [CULLong] -> [CULLong] #-}
getRuns'A :: (Num a, Num b, Eq a) => b -> [a] -> [b]
getRuns'A _   []    = []
getRuns'A n  (1:xs) = n : getRunsA        xs
getRuns'A n ~(_:xs) =     getRuns'A (n+1) xs

-- | A "match [] last" version of `getRuns'`
-- {-# SPECIALIZE getRuns'B :: CULLong -> [CULLong] -> [CULLong] #-}
getRuns'B :: (Num a, Num b, Eq a) => b -> [a] -> [b]
getRuns'B n (1:xs) = n : getRunsB        xs
getRuns'B n (_:xs) =     getRuns'B (n+1) xs
getRuns'B _  _     = []


-- -- | More readable (and specialized) than @zip [0..]@
-- {-# SPECIALIZE notePosition :: [CULLong] -> [(CULLong, CULLong)] #-}
-- notePosition :: (Enum a, Num a) => [b] -> [(a, b)]
-- notePosition = zip [0..]


-- addPosition :: (CULLong, CULLong)                                              -- ^ (position, value)
--             -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (last input position, list of its previous runs)
--             -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (new  input position, list of its previous runs)
addPosition :: (Ord k0, Eq a, Num a) => (a, k0) -> Map k0 ((a, a, a, a, a), [(a, a, a, a, a)]) -> Map k0 ((a, a, a, a, a), [(a, a, a, a, a)])
addPosition (position, value) posMap = Map.insertWith posValInsert value ((position,0,0,0,0),[]) posMap

-- Some code, I believe an alternate attempt/pseudocode/practice
-- @
-- posValInsert ((pos5,_,_,_),_) ((pos1,pos2,pos3,pos4), pastRuns) = if newRun `elem` pastRuns
--                                                          then (positions,          pastRuns)
--                                                          else (positions, newRun : pastRuns)
--   where
--     newRun    = (pos1,pos2,pos3,pos4,pos5)
--     positions = (     pos2,pos3,pos4,pos5)
-- @
posValInsert :: (Eq a, Num a) => ((a,a,a,a,a),[(a,a,a,a,a)]) ->
                         ((a,a,a,a,a),[(a,a,a,a,a)]) ->
                         ((a,a,a,a,a),[(a,a,a,a,a)])

posValInsert ((pos6,_,_,_,_),_) ((pos1,pos2,pos3,pos4,pos5), pastRuns) = if newRun `elem` pastRuns || 0 `elem` [pos1,pos2,pos3,pos4,pos5,pos6]
                                                                            then (positions,          pastRuns)
                                                                            else (positions, newRun : pastRuns)
  where
    -- newRun    :: (CULLong, CULLong, CULLong, CULLong, CULLong)
    newRun    = (pos2-pos1,pos3-pos2,pos4-pos3,pos5-pos4,pos6-pos5)

    -- positions :: (CULLong, CULLong, CULLong, CULLong, CULLong)
    positions = (pos2     ,pos3     ,pos4     ,pos5     ,pos6     )





printMap :: forall a1 a b c. (Show b, Show c) => (a1, Map b (a, [(c, c, c, c, c)])) -> IO ()
printMap = putStrLn . showTreeWith (\k x -> show (k, snd x)) True False . snd


----- Ahhhhh need differences in positions of differences..

-- getRuns $ differences $ onlyGapless $ [1..]
--  ^ this is what to extract positions of 1s, 2s.. and then take the differences of those positions and store those runs...



-- {-# SPECIALIZE theStream :: (Bits a1, Num a1, Enum a1) => ([(CULLong, CULLong)], Map a1 a1) #-}
-- theStream :: (Num w, Enum w, Bits w, Bits a, Num a, Enum a) => ([(w, w)], Map a a)
-- theStream :: (Num k, Bits k, Enum k, Num a, Bits a, Enum a) => ([(CULLong, CULLong)], Map k a)
-- theStream = baseMap . notePosition . getRuns . differences . onlyGapless $ [1..]
theStream :: (Bits a1, Num a1, Enum a1) => ([(w, w)], Map a1 a1)
theStream = error "stopgap"
--   where
--     -- baseMap :: a -> (a, Map k' a1')
--     baseMap = flip (,) Map.empty


-- printEveryNOfStream :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)]))
printEveryNOfStream n = error "stopgep" -- = printEveryN (foldlOnce addPosition) n theStream

-- returnEveryN :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)]))
returnEveryN n = error "stopgep" -- sideEffectEveryN printMap (foldlOnce addPosition) n theStream


-- get runs of 1's, 4^n's
-- drop runs of 1's
-- return runs of 4^n's as maximum power (Int)
-- put positions of n's into map
-- convert list of positions into runs of 5 (7)?
-- nub the runs




