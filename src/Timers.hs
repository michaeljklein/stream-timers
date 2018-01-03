
module Timers
    ( foldrOnce,
      foldlOnce,
      foldrTimes,
      foldlTimes,
      foldrTimesM,
      foldlTimesM,
      nest,
      nestM,
      nestByM,
      nestMForever,
      switchByPred,
      switchOnM,
      switchEveryN,
      switchEveryM,
      fixPt,
      printEveryN,
      sideEffectEveryN,
      switchEvery',
      switchEvery''
    ) where


import Control.Monad (foldM, liftM, liftM2)
import Data.Time.Clock (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Data.Void


-- reallyNoWay                             :: Eq a => (a -> a) -> a -> a
-- reallyNoWay = (\f x -> if f x == x then x else fix f)

-- reallyNoWay $ reallyNoWay               :: (Eq a, Eq (a -> a)) => (a -> a) -> a -> a
-- reallyNoWay $ reallyNoWay $ reallyNoWay :: (Eq a, Eq (a -> a)) => (a -> a) -> a -> a
-- -- This is an ouroboros function: it's own, input and output types are all equal

-- | `nest` applies f to x, n times.
nest :: (b -> b) -> b -> Int -> b
nest f x n = foldr (f.) x (replicate n id)

-- | `nestM` generalizes `nest` to functions returning Monad values.
nestM :: Monad m => (b -> m b) -> b -> Int -> m b
nestM f x n = foldM ((. const) . (.) $ f) x (replicate n id)

-- | `nestByM` generalizes `nestM` to numbers inside monads (for example, `io` could be (read.getLine)).
nestByM :: (Monad m, Monad t, Foldable t) => (b -> m b) -> b -> t Int -> m b
nestByM f x io = foldM ((. const) . (.) $ f) x (liftM (`replicate` id) io)

-- | `nestMForever` is equivalent to `nestM f x Infinity`.
nestMForever :: Monad m => (b -> m b) -> b -> m b
nestMForever f x = foldM ((. const) . (.) $ f) x (repeat id)

-- | `foldrOnce` applies a right fold, only once (for streams, primarily).
foldrOnce :: (a -> b -> a) -> (a, [b]) -> (a, [b])
foldrOnce _  (x, []    ) = (  x  , [])
foldrOnce f ~(x, y : zs) = (f x y, zs)

-- | `foldrTimes` applies `foldrOnce` a given number of times.
foldrTimes :: (a -> b -> a) -> (a, [b]) -> Int -> (a, [b])
foldrTimes = nest . foldrOnce

-- | `foldlOnce` is the left-associative version of `foldrOnce`.
foldlOnce :: (a -> b -> b) -> ([a], b) -> ([a], b)
foldlOnce _ ([]    , x) = ([],     x)
foldlOnce f (y : zs, x) = (zs, f y x)

-- | See `foldlOnce`, `foldrTimes`.
foldlTimes :: (a -> b -> b) -> ([a], b) -> Int -> ([a], b)
foldlTimes = nest . foldlOnce

-- | `foldrOnceM` generalizes `foldrOnceM` to general monads.
foldrOnceM :: Monad m => (a -> b -> a) -> (a, [b]) -> m (a, [b])
foldrOnceM _ (x, []) = return (x, [])
foldrOnceM f (x, y:zs) = return (f x y, zs)

-- | See `foldrOnceM`, `foldrTimes`.
foldrTimesM :: Monad m => (t -> t1 -> t) -> (t, [t1]) -> Int -> m (t, [t1])
foldrTimesM = nestM . foldrOnceM

-- | See `foldrOnceM`, `foldlOnce`.
foldlOnceM :: Monad m => (t1 -> t -> t) -> ([t1], t) -> m ([t1], t)
foldlOnceM _ ([]    , x) = return ([],     x)
foldlOnceM f (y:zs, x) = return (zs, f y x)

-- | See `foldrTimesM`, `foldlOnceM`,
foldlTimesM :: Monad m => (t1 -> t -> t) -> ([t1], t) -> Int -> m ([t1], t)
foldlTimesM = nestM . foldlOnceM

-- | `switchByPred` returns `f x` if `p x` else `g x`.
switchByPred :: (t1 -> t) -> (t1 -> t) -> (t1 -> Bool) -> t1 -> t
switchByPred f g p x = if p x then f x else g x

-- | `switchEveryN` applies f, n times, then g once, then repeats forever.
switchEveryN :: Monad m => (b -> b) -> (b -> m b) -> Int -> b -> m b
switchEveryN f g n = nestMForever (g . flip (nest f) n)

-- | `switchOnM` is to `switchEveryN` as `nestByM` is to `nestM`.
switchOnM :: (Monad m, Monad m1, Monad t, Foldable t) => (b -> m1 b) -> (m1 b -> m b) -> t Int -> b -> m b
switchOnM f g m = nestMForever (g . flip (nestByM f) m)

-- Next, need version of switchOnM that recurses for m.., i.e. [m x, m$m x, m$m$m x..]



-- | `fixPt` gives the fixed point of f on x.
fixPt :: Eq a => (a -> a) -> a -> a
fixPt = until =<< ((==) <*>)



-- now we actually get to the printing...

printEveryN :: Show b => (b -> b) -> Int -> b -> IO b
printEveryN = sideEffectEveryN print --switchEveryN f ((liftM2 (>>) print return) . f)

sideEffectEveryN :: Monad m => (b -> m a) -> (b -> b) -> Int -> b -> m b
sideEffectEveryN s f = switchEveryN f ((liftM2 (>>) s return) . f)



-- -- | This function assumes that applying `f` too many times is fine, it tries to apply `f` just enough times to hit EQ, then applies `g` and repeats forever
-- nestByOrd :: (a -> Ordering) -> (a -> a) -> (a -> a) -> a -> a
-- nestByOrd o f g x = nestByOrd' (or,n0,n1) o f g x'
--   where
--     or = o x'
--     n0 = 0
--     n1 = 2
--     x' = nest f (g x) 2

-- nestByOrd' (or,n0,n1) o f g x = nestByOrd' (or',n1,n2) o f g $ nest f (g x) n2
--   where
--     or'   = o x
--     delta = div (n0 + n1) 3 * (ordToInt or + ordToInt or')
--     n2    = max (n1 + delta) 1
-- -- 3 is a magic number, so delta is less than the mean but not by much (otherwise, would be more trouble to ensure convergence for 'nice' functions)
-- -- 1 is a magic number, so that the function






-- | seconds taken to be in [1..59]. This returns fElse of the input if the clock time's minutes == minutes, otherwise f x.
-- Note that this function results in an infinite loop
switchEveryM  :: Int -> Int -> (a -> IO a) -> (a -> IO a) -> a -> IO Void
switchEveryM goalTime iterations f fElse x = do
  currentTime <- getCurrentTime
  loop currentTime iterations (return x)
    where
      loop lastTime lastNumIter x0 = do
        currentTime   <- getCurrentTime
        let timeDiff  =  diffUTCTime currentTime lastTime
        let meanTime  =  timeDiff / (fromIntegral lastNumIter)
        let newIter   =  round $ fromIntegral goalTime / meanTime
        x1            <- x0
        x2            <- fElse x1
        x3            <- nestM f x2 newIter
        loop currentTime newIter (return x3)


-- | This is a sketch of what a "log every so often, efficiently" function could look like.
switchEvery'' :: (Integral t4, Integral t6) => (UTCTime, t6, t4, t7 -> IO t7, t5 -> IO t7, IO t5) -> IO (UTCTime, t6, Int, t7 -> IO t7, t5 -> IO t7, t7)
switchEvery'' (lastTime, goalTime, lastNumIter, f, fElse, x0) = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime lastTime
  let meanTime = timeDiff / fromIntegral lastNumIter
  let newIter  = round $ fromIntegral goalTime / meanTime
  x1 <- x0
  x2 <- fElse x1
  x3 <- nestM f x2 newIter
  return (currentTime, goalTime, newIter, f, fElse, x3)

switchEvery' :: (UTCTime, Integer, Int, IO t5 -> IO (IO t5), t5 -> IO (IO t5), IO t5) -> IO (UTCTime, Integer, Int, IO t5 -> IO (IO t5), t5 -> IO (IO t5), IO t5)
switchEvery' = nestMForever switchEvery''

