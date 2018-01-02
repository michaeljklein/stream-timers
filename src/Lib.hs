module Lib
    ( gapless,
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
      returnEveryN,
      realMain
    ) where

-- import Control.Monad (foldM_)
import Data.Bits ((.&.), unsafeShiftR)
import Foreign.C.Types (CULLong)
import qualified Data.Map.Strict as Map
import Timers (foldlOnce, sideEffectEveryN) -- foldrTimes,
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

-- | Warning, this function is incomplete.
getRuns  ::            [ULL] -> [ULL]
getRuns    (1:xs) =     getRuns        xs
getRuns    (_:xs) =     getRuns'  1    xs
getRuns     _     = error "getRuns: [] is undefined"

-- | A "match [] first" version of `getRuns`
getRunsA  ::            [ULL] -> [ULL]
getRunsA      []    =     []
getRunsA     (1:xs) =     getRunsA        xs
getRunsA    ~(_:xs) =     getRuns'A  1    xs

-- | A "match [] last" version of `getRuns`
getRunsB  ::            [ULL] -> [ULL]
getRunsB    (1:xs) =     getRunsB        xs
getRunsB    (_:xs) =     getRuns'B  1    xs
getRunsB     _     =     []


-- | Warning, this function is incomplete.
getRuns' :: ULL -> [ULL] -> [ULL]
getRuns' n (1:xs) = n : getRuns        xs
getRuns' n (_:xs) =     getRuns' (n+1) xs
getRuns' _  _     = error "getRuns' _ [] is undefined"

-- | A "match [] first" version of `getRuns'`
getRuns'A :: ULL -> [ULL] -> [ULL]
getRuns'A _   []    = []
getRuns'A n  (1:xs) = n : getRunsA        xs
getRuns'A n ~(_:xs) =     getRuns'A (n+1) xs

-- | A "match [] last" version of `getRuns'`
getRuns'B :: ULL -> [ULL] -> [ULL]
getRuns'B n (1:xs) = n : getRunsB        xs
getRuns'B n (_:xs) =     getRuns'B (n+1) xs
getRuns'B _  _     = []


-- | More readable (and specialized) than @zip [0..]@
notePosition :: [ULL] -> [(ULL, ULL)]
notePosition = (zip [0..] :: (Enum a, Num a) => [b] -> [(a, b)])

addPosition :: (ULL, ULL)                                              -- ^ (position, value)
            -> Map ULL ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) -- ^ Map Value (last input position, list of its previous runs)
            -> Map ULL ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) -- ^ Map Value (new  input position, list of its previous runs)
addPosition (position, value) posMap = Map.insertWith posValInsert value ((position,0,0,0,0),[]) posMap


posValInsert :: ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) ->
                ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)]) ->
                ((ULL,ULL,ULL,ULL,ULL),[(ULL,ULL,ULL,ULL,ULL)])

posValInsert ((pos6,_,_,_,_),_) ((pos1,pos2,pos3,pos4,pos5), pastRuns) = if newRun `elem` pastRuns || 0 `elem` [pos1,pos2,pos3,pos4,pos5,pos6]
                                                                            then (positions,          pastRuns)
                                                                            else (positions, newRun : pastRuns)
  where
    newRun    :: (ULL, ULL, ULL, ULL, ULL)
    newRun    = (pos2-pos1,pos3-pos2,pos4-pos3,pos5-pos4,pos6-pos5)

    positions :: (ULL, ULL, ULL, ULL, ULL)
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



-- returnEveryN n = printMapEveryN (foldlOnce addPosition) n theStream

-- returnEveryN n = printEveryN (foldlOnce addPosition) n theStream

returnEveryN :: Int -> IO ([(ULL, ULL)], Map ULL ((ULL, ULL, ULL, ULL, ULL), [(ULL, ULL, ULL, ULL, ULL)]))
returnEveryN n = sideEffectEveryN printMap (foldlOnce addPosition) n theStream

realMain :: IO ()
realMain = do
  args <- getArgs
  let arg = if null args then (error "no arguements passed! Please provide a somewhat large number.") else head args
  returns <- returnEveryN (read arg :: Int)
  print $ returns



-- get runs of 1's, 4^n's
-- drop runs of 1's
-- return runs of 4^n's as maximum power (Int)
-- put positions of n's into map
-- convert list of positions into runs of 5 (7)?
-- nub the runs




