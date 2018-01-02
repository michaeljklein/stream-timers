module Lib
    ( gapless,
      nextGapless,
      onlyGapless,
      differences,
      getRuns,
      addPosition,
      theStream,
      printMap,
      returnEveryN,
      realMain
    ) where

import Control.Monad (foldM_)
import Data.Bits ((.&.), unsafeShiftR)
import Foreign.C.Types (CULLong)
import qualified Data.Map.Strict as Map
import Timers (foldrTimes, foldlOnce, sideEffectEveryN)
import System.Environment (getArgs)

type ULL = CULLong
type Map = Map.Map

emptyMap :: Map k a
emptyMap = Map.empty

gapless :: ULL -> Bool
gapless z = if z == 0
               then True
               else if 3 .&. z == 0              -- if mod z 4 == 0
               then gapless  $ unsafeShiftR z 2  -- gapless (div z 2)
               else gapless' $ unsafeShiftR z 2

gapless' :: ULL -> Bool
gapless' z = if z == 0
                then True
                else if 3 .&. z == 0
                then False
                else gapless' $ unsafeShiftR z 2

nextGapless :: ULL -> ULL
nextGapless = until gapless (+1)

streamTail ::          [ULL]
streamTail = [1..]

onlyGapless ::         [ULL] -> [ULL]
onlyGapless = filter gapless

differences ::         [ULL] -> [ULL]
differences = zipWith (-) =<< tail

getRuns  ::            [ULL] -> [ULL]
getRuns    (1:xs) =     getRuns        xs
getRuns    (x:xs) =     getRuns'  1    xs

getRuns' :: ULL -> [ULL] -> [ULL]
getRuns' n (1:xs) = n : getRuns        xs
getRuns' n (_:xs) =     getRuns' (n+1) xs



notePosition :: [ULL] -> [(ULL, ULL)]
notePosition = zip [0..]

--             position  value               value    lst pos   lst pos2 lst pos3 lst pos4   list of runs
addPosition :: (ULL,ULL) -> Map ULL ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) -> Map ULL ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)])
addPosition (position, value) posMap = Map.insertWith posValInsert value ((position,0,0,0,0),[]) posMap


posValInsert :: ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) ->
                ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) ->
                ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)])

posValInsert ((pos6,_,_,_,_),_) ((pos1,pos2,pos3,pos4,pos5), pastRuns) = if newRun `elem` pastRuns || 0 `elem` [pos1,pos2,pos3,pos4,pos5,pos6]
                                                                            then (positions,          pastRuns)
                                                                            else (positions, newRun : pastRuns)
  where
    newRun    = (pos2-pos1,pos3-pos2,pos4-pos3,pos5-pos4,pos6-pos5)
    positions = (pos2     ,pos3     ,pos4     ,pos5     ,pos6     )
-- posValInsert ((pos5,_,_,_),_) ((pos1,pos2,pos3,pos4), pastRuns) = if newRun `elem` pastRuns
--                                                          then (positions,          pastRuns)
--                                                          else (positions, newRun : pastRuns)
--   where
--     newRun    = (pos1,pos2,pos3,pos4,pos5)
--     positions = (     pos2,pos3,pos4,pos5)


baseMap :: a -> (a, Map k a1)
baseMap = flip (,) emptyMap

printMap :: (a1, Map ULL (a, [(ULL, ULL, ULL, ULL, ULL)])) -> IO ()
printMap = putStrLn . Map.showTreeWith (\k x -> show (k, snd x)) True False . snd

----- Ahhhhh need differences in positions of differences..

--getRuns $ differences $ onlyGapless $ [1..]
-- ^ this is what to extract positions of 1s, 2s.. and then take the differences of those positions and store those runs...



theStream :: ([(ULL, ULL)], Lib.Map k a1)
theStream = baseMap . notePosition . getRuns . differences . onlyGapless $ streamTail

returnEveryN :: Int -> IO ([(ULL, ULL)], Map ULL ((ULL, ULL, ULL, ULL, ULL), [(ULL, ULL, ULL, ULL, ULL)]))
returnEveryN n = sideEffectEveryN printMap (foldlOnce addPosition) n theStream

realMain = do
  args <- getArgs
  let arg = if null args then (error "no arguements passed! Please provide a somewhat large number.") else head args
  returns <- returnEveryN (read arg :: Int)
  print $ returns


-- returnEveryN n = printMapEveryN (foldlOnce addPosition) n theStream

-- returnEveryN n = printEveryN (foldlOnce addPosition) n theStream



-- get runs of 1's, 4^n's
-- drop runs of 1's
-- return runs of 4^n's as maximum power (Int)
-- put positions of n's into map
-- convert list of positions into runs of 5 (7)?
-- nub the runs

