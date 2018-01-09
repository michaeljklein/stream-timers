{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Timers.Example
    ( Map,
      gapless,
      gapless',
      gaplessEq,
      countGapless,
      countGapless',
      nextGapless,
      toMathematica,
      onlyGapless,
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
      Vec2,
      Vec4,
      Vec5,
      addPosition',
      theStream,
      printMap,
      printMap',
      printEveryNOfStream,
      printEveryNOfStream',
      returnEveryN,
      returnEveryN'
    ) where


import Data.Bits (Bits, (.&.), unsafeShiftR)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Timers (foldlOnce, sideEffectEveryN, printEveryN) -- foldrTimes,
import Control.Monad (liftM2)
import Data.Map.Internal.Debug (showTreeWith)
import Data.List (group)

import Data.Vector.Fixed.Cont (ContVec, mk5)
import Data.Vector.Fixed ()
import qualified Data.Vector.Fixed as FV



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

-- | `onlyGapless`, differences, grouped, then converted to Mathematica lists.
-- {-# SPECIALIZE onlyGaplessMathematica ::         [CULLong] -> [Char] #-}
onlyGaplessMathematica :: (Bits t, Num t, Show t) => [t] -> String
onlyGaplessMathematica = toMathematica . group . differences . onlyGapless

-- | Convert a list of lists to a list of Mathematica lists.
-- {-# SPECIALIZE toMathematica ::         [[CULLong]] -> [Char] #-}
toMathematica :: Show a => [[a]] -> String
toMathematica = (>>= (\x->concat["{",show(head x),",",show(length x),"},"]))


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

-- | Compare `differences` and `differences_`, specialized for testing
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


-- addPosition :: (CULLong, CULLong)                                              -- ^ (position, value)
--             -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (last input position, list of its previous runs)
--             -> Map CULLong ((CULLong,CULLong,CULLong,CULLong,CULLong),[(CULLong,CULLong,CULLong,CULLong,CULLong)]) -- ^ Map Value (new  input position, list of its previous runs)
addPosition :: (Ord k0, Eq a, Num a) => (a, k0)
                                     -> Map k0 ((a, a, a, a, a), [(a, a, a, a, a)])
                                     -> Map k0 ((a, a, a, a, a), [(a, a, a, a, a)])
addPosition (position, value) posMap = Map.insertWith posValInsert value ((position,0,0,0,0),[]) posMap



type Vec2 = ContVec 2
type Vec4 = ContVec 4
type Vec5 = ContVec 5


-- | A version of `addPosition` using `Vec5` instead of tuples
addPosition' :: (Ord k0, Eq a, Num a) => a -> k0 -> Map k0 (Vec5 a, [Vec5 a]) -> Map k0 (Vec5 a, [Vec5 a])
addPosition' position value posMap = Map.insertWith posValInsert' value (mk5 position 0 0 0 0, []) posMap


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


-- | A version of `posValInsert` using `Vec4` and `Vec5` instead of tuples
posValInsert' :: (Eq a, Num a) => (Vec5 a, [Vec5 a]) -> (Vec5 a, [Vec5 a]) -> (Vec5 a, [Vec5 a])
posValInsert' ~((x :: Vec5 a), _) ~(y, zs) = if any (FV.and . FV.zipWith (==) newRun) zs || 0 == sixth || 0 `elem` y
                                     then (positions,          zs)
                                     else (positions, newRun : zs)
  where
    sixth     = FV.head x
    temp      = (FV.tail :: Vec5 a -> Vec4 a) y -- Vec4
    positions = FV.snoc sixth temp -- Vec5
    newRun    = FV.zipWith (-) positions y -- Vec5





-- | Print the resulting `Map`
printMap :: forall a1 a b c. (Show b, Show c) => (a1, Map b (a, [(c, c, c, c, c)])) -> IO ()
printMap = putStrLn . showTreeWith (\k x -> show (k, snd x)) True False . snd

-- | Print the resulting `Map`, specialized to `Vec5`'s instead of tuples
printMap' :: (Show a0, Show a2) => (a, Map a0 (a1, [Vec5 a2])) -> IO ()
printMap' = (putStrLn . showTreeWith (\k ~(_, xs) -> show (k, FV.toList <$> xs)) True False . snd)


----- Ahhhhh need differences in positions of differences..

-- getRuns $ differences $ onlyGapless $ [1..]
--  ^ this is what to extract positions of 1s, 2s.. and then take the differences of those positions and store those runs...


-- | The initial state of the stream
-- {-# SPECIALIZE theStream :: (Bits a1, Num a1, Enum a1) => ([(CULLong, CULLong)], Map a1 a1) #-}
theStream :: (Enum a, Enum b, Num b, Bits b, Num a) => ([(a, b)], Map k v)
theStream = (, Map.empty) . zip [1..] . lock getRuns . differences . lock onlyGapless $ [1..]
  where
    lock :: ([t] -> [t]) -> [t] -> [t]
    lock = id

-- | Print every nth value of the stream, specialized to the following value (it's an infinite loop, so the return value simply encodes which types the algorithm is specialized to).
-- {-# SPECIALIZE printEveryNOfStream :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)])) #-}
printEveryNOfStream :: (Eq a, Bits k0, Num k0, Num a, Enum k0, Enum a, Ord k0, Show k0, Show a) =>
     Int -> IO ([(a, k0)], Map k0 ((a, a, a, a, a), [(a, a, a, a, a)]))
printEveryNOfStream n = printEveryN (foldlOnce addPosition) n theStream

-- | `printEveryNOfStream`, secialized to `ContVec`'s and using `addPosition'`
printEveryNOfStream' :: (Eq a, Bits k0, Num k0, Num a, Enum k0, Enum a, Ord k0, Show (ContVec 5 a), Show k0, Show a) =>
     Int -> IO ([(a, k0)], Map k0 (Vec5 a, [Vec5 a]))
printEveryNOfStream' n = printEveryN (foldlOnce (uncurry addPosition')) n theStream


-- returnEveryN :: Int -> IO ([(CULLong, CULLong)], Map CULLong ((CULLong, CULLong, CULLong, CULLong, CULLong), [(CULLong, CULLong, CULLong, CULLong, CULLong)]))
-- | See `printEveryNOfStream`, except is uses `sideEffectEveryN` instead of `printEveryN`
returnEveryN :: (Eq c, Show b, Show c, Ord b, Enum c, Enum b, Num c, Num b, Bits b) => Int -- ^ How often
             -> IO ([(c, b)], Map b ((c, c, c, c, c), [(c, c, c, c, c)])) -- ^ Results from `theStream`
returnEveryN n = sideEffectEveryN printMap (foldlOnce addPosition) n theStream

-- | A version of `returnEveryN` using `addPosition'` and `printMap'`
returnEveryN' :: (Eq a2, Enum a2, Num a2, Bits a1, Enum a1, Num a1, Ord a1, Show a2, Show a1) => Int  -- ^ How often
              -> IO ([(a2, a1)], Map a1 (Vec5 a2, [ContVec 5 a2])) -- ^ Results from `theStream`
returnEveryN' n = sideEffectEveryN printMap' (foldlOnce (uncurry addPosition')) n theStream



-- get runs of 1's, 4^n's
-- drop runs of 1's
-- return runs of 4^n's as maximum power (Int)
-- put positions of n's into map
-- convert list of positions into runs of 5 (7)?
-- nub the runs




