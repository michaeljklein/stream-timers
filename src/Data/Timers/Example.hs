{-# LANGUAGE RankNTypes #-}

module Data.Timers.Example
    ( CULLong,
      Map,
      gapless,
      gapless',
      gaplessEq,
      countGapless,
      countGapless',
      nextGapless,
      onlyGapless,
      differences,
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


import Data.Bits ((.&.), unsafeShiftR)
import Foreign.C.Types (CULLong)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Timers (foldlOnce, sideEffectEveryN, printEveryN) -- foldrTimes,
import Control.Monad (liftM2)
import Data.Map.Internal.Debug (showTreeWith)


-- | Mystery function... (read the source)
gapless :: CULLong -> Bool
gapless z = if z == 0
               then True
               else if 3 .&. z == 0              -- if mod z 4 == 0
               then gapless  $ unsafeShiftR z 2  -- gapless (div z 2)
               else gapless' $ unsafeShiftR z 2

-- | Helper function to `gapless`
gapless' :: CULLong -> Bool
gapless' z = if z == 0
                then True
                else if 3 .&. z == 0
                then False
                else gapless' $ unsafeShiftR z 2


-- | Number of `gapless` numbers in @[1..n]@
countGapless :: Num a => CULLong -> a
countGapless n = sum [ sum [ 1 | gapless i] | i<-[1..n]]

-- | Number of `gapless'` numbers in @[1..n]@
countGapless' :: Num a => CULLong -> a
countGapless' n = sum [ sum [ 1 | gapless' i] | i<-[1..n]]

-- | Are `gapless` and `gapless'` equal?
gaplessEq :: CULLong -> Bool
gaplessEq = liftM2 (==) gapless gapless'



-- | What it says on the box
nextGapless :: CULLong -> CULLong
nextGapless = until gapless (+1)

-- | Easier to read and more specialized than @[1..]@
streamTail ::          [CULLong]
streamTail = [1..]


-- | One-liner to export gapless to mathematica:
--
-- @
--  concatMap (\x->concat["{",show(head x),",",show(length x),"},"]).group . (\x->zipWith(-)(tail x)x) . onlyGapless $ [1..100]
-- @
--
-- See @only_gapless.md@
onlyGapless ::         [CULLong] -> [CULLong]
onlyGapless = filter gapless

-- | First differences:
--
-- @
--  [x0, x1, x2, x3..] -> [x1 - x0, x2 - x1, x3 - x2..]
-- @
differences :: Num a => [a] -> [a]
differences = zipWith (-) =<< tail


-- | Warning, this function is incomplete.
getRuns  ::            [CULLong] -> [CULLong]
getRuns    (1:xs) =     getRuns        xs
getRuns    (_:xs) =     getRuns'  1    xs
getRuns     _     = error "getRuns: [] is undefined"

-- | A "match [] first" version of `getRuns`
getRunsA  ::            [CULLong] -> [CULLong]
getRunsA      []    =     []
getRunsA     (1:xs) =     getRunsA        xs
getRunsA    ~(_:xs) =     getRuns'A  1    xs

-- | A "match [] last" version of `getRuns`
getRunsB  ::            [CULLong] -> [CULLong]
getRunsB    (1:xs) =     getRunsB        xs
getRunsB    (_:xs) =     getRuns'B  1    xs
getRunsB     _     =     []


-- | Warning, this function is incomplete.
getRuns' :: CULLong -> [CULLong] -> [CULLong]
getRuns' n (1:xs) = n : getRuns        xs
getRuns' n (_:xs) =     getRuns' (n+1) xs
getRuns' _  _     = error "getRuns' _ [] is undefined"

-- | A "match [] first" version of `getRuns'`
getRuns'A :: CULLong -> [CULLong] -> [CULLong]
getRuns'A _   []    = []
getRuns'A n  (1:xs) = n : getRunsA        xs
getRuns'A n ~(_:xs) =     getRuns'A (n+1) xs

-- | A "match [] last" version of `getRuns'`
getRuns'B :: CULLong -> [CULLong] -> [CULLong]
getRuns'B n (1:xs) = n : getRunsB        xs
getRuns'B n (_:xs) =     getRuns'B (n+1) xs
getRuns'B _  _     = []


-- | More readable (and specialized) than @zip [0..]@
notePosition :: [CULLong] -> [(CULLong, CULLong)]
notePosition = (zip [0..] :: (Enum a, Num a) => [b] -> [(a, b)])

addPosition :: (CULLong, CULLong)                                              -- ^ (position, value)
            -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (last input position, list of its previous runs)
            -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (new  input position, list of its previous runs)
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
posValInsert :: ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) ->
                ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) ->
                ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)])

posValInsert ((pos6,_,_,_,_),_) ((pos1,pos2,pos3,pos4,pos5), pastRuns) = if newRun `elem` pastRuns || 0 `elem` [pos1,pos2,pos3,pos4,pos5,pos6]
                                                                            then (positions,          pastRuns)
                                                                            else (positions, newRun : pastRuns)
  where
    newRun    :: (CULLong, CULLong, CULLong, CULLong, CULLong)
    newRun    = (pos2-pos1,pos3-pos2,pos4-pos3,pos5-pos4,pos6-pos5)

    positions :: (CULLong, CULLong, CULLong, CULLong, CULLong)
    positions = (pos2     ,pos3     ,pos4     ,pos5     ,pos6     )





printMap :: forall a1 a. (a1, Map CULLong (a, [(CULLong, CULLong, CULLong, CULLong, CULLong)])) -> IO ()
printMap = putStrLn . showTreeWith (\k x -> show (k, snd x)) True False . snd

----- Ahhhhh need differences in positions of differences..

-- getRuns $ differences $ onlyGapless $ [1..]
--  ^ this is what to extract positions of 1s, 2s.. and then take the differences of those positions and store those runs...



theStream :: ([(CULLong, CULLong)], Map k a1)
theStream = baseMap . notePosition . getRuns . differences . onlyGapless $ streamTail
  where
    baseMap :: a -> (a, Map k' a1')
    baseMap = flip (,) Map.empty


printEveryNOfStream :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)]))
printEveryNOfStream n = printEveryN (foldlOnce addPosition) n theStream

returnEveryN :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)]))
returnEveryN n = sideEffectEveryN printMap (foldlOnce addPosition) n theStream


-- get runs of 1's, 4^n's
-- drop runs of 1's
-- return runs of 4^n's as maximum power (Int)
-- put positions of n's into map
-- convert list of positions into runs of 5 (7)?
-- nub the runs




