
module Timers
    ( foldrOnce,
      foldrTimes,
      foldlOnce,
      foldlTimes,
      foldrOnceM,
      foldrTimesM,
      foldlOnceM,
      foldlTimesM,
      nest,
      nestM,
      nestByM,
      nestMForever,
      nestByOrd,
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
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Void


-- reallyNoWay                             :: Eq a => (a -> a) -> a -> a
-- reallyNoWay = (\f x -> if f x == x then x else fix f)
--
-- reallyNoWay $ reallyNoWay               :: (Eq a, Eq (a -> a)) => (a -> a) -> a -> a
-- reallyNoWay $ reallyNoWay $ reallyNoWay :: (Eq a, Eq (a -> a)) => (a -> a) -> a -> a
--
-- This is (my attempt at) an ouroboros function: it's own, input and output types are all equal,
-- assuming you can find a way to find equality between arbitrary functions of values with equality.


-- | `nest` applies f to x, n times.
nest :: (b -> b) -> b -> Int -> b
nest f x n = foldr (f.) x (replicate n id)

-- | `nestM` generalizes `nest` to functions returning Monad values.
nestM :: Monad m => (b -> m b) -> b -> Int -> m b
nestM f x n = foldM ((. const) . (.) $ f) x (replicate n id)

-- | `nestByM` generalizes `nestM` to numbers inside monads (for example, @mn@ could be (read.getLine)).
nestByM :: Monad m => (b -> m b) -> b -> m Int -> m b
nestByM f x mn = foldM (\y z -> z f y) x =<< liftM (flip replicate ($)) mn


-- | `nestMForever` is equivalent to `nestM f x Infinity`.
--
-- @
-- λ> nestMForever (\x -> print x >> return x) (return 10 :: Expire Int)
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {ge^Cpire = (1,Just 10)}
-- Expire {getExpire = (1,Just 10)}
-- Expire {getExpireInterrupted.
-- @
--
-- @
-- λ> nestMForever (\x -> print x >> return x) (return 10 :: ExpireE Int)
-- ExpireE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}
-- Ex^CE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}
-- ExpireE {getExpireE = (1,10)}Interrupted.
-- @
--
-- @
-- λ> dumpExpireIO =<< nestMForever (id $ \x -> (dumpExpireIO x >>= print) >> return x) (return 10 :: ExpireIO Int)
-- J^Cust (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1Interrupted.
-- @
--
-- @
-- λ> dumpExpireIO =<< nestMForever (id $ \x -> (dumpExpireIO x >>= print >> killExpireIO x) >> return x) (return 10 :: ExpireIO Int)
-- Just^Cust (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10Interrupted.
-- @
--
-- @
-- λ> dumpExpireIO =<< nestMForever (id $ \x -> (dumpExpireIO x >>= print) >> return x) (return 10 :: ExpireIO Int)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Just (1,10)
-- Jus^Ct (1,10)
-- JusInterrupted.
--
-- Real Mem: 157.4 MB
--
-- See `stable_infinite_ExpireIO_loop_sample.txt` for a sample
-- @
--
nestMForever :: Monad m => (b -> m b) -> b -> m b
nestMForever f x = foldM ((. const) . (.) $ f) x (repeat id)



-- | This function assumes that applying `f` too many times is fine, it tries to apply `f` just enough times to hit EQ, then applies `g` and repeats forever
nestByOrd :: (a -> Ordering) -> (a -> a) -> (a -> a) -> a -> a
nestByOrd o f g x = nestByOrd' (or_,n0,n1) o f g x'
  where
    or_ = o x' :: Ordering
    n0 = 0
    n1 = 2
    x' = nest f (g x) 2

-- | Helper function to `nestByOrd`
nestByOrd' :: (Ordering, Int, Int) -> (t -> Ordering) -> (t -> t) -> (t -> t) -> t -> t1
nestByOrd' (or_,n0,n1) o f g x = nestByOrd' (or',n1,n2) o f g $ nest f (g x) n2
  where
    or'   = o x :: Ordering
    delta = div (n0 + n1) 3 * (ordToInt or_ + ordToInt or')
    n2    = max (n1 + delta) 1
    ordToInt = fromEnum
    -- 3 is a magic number, so delta is less than the mean but not by much (otherwise, would be more trouble to ensure convergence for 'nice' functions)




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
switchOnM :: (Monad m, Monad m1) => (b -> m1 b) -> (m1 b -> m b) -> m1 Int -> b -> m b
switchOnM f g m = nestMForever (g . flip (nestByM f) m)


-- Next, need version of switchOnM that recurses for m.., i.e. [m x, m$m x, m$m$m x..]



-- | `fixPt` gives the fixed point of @f@ on @x@.
-- Compare its type to that of `fix`:
--
-- @
--  fix :: (a -> a) -> a
-- @
--
-- This fixed-point stops at equality, e.g. @1, 2, 3, 3, a.. -> 1, 2, 3@.
fixPt :: Eq a => (a -> a) -> a -> a
fixPt = until =<< ((==) <*>)



-- now we actually get to the printing...

-- | Nest the function the given number of times, on the given value, printing every iteration
--
-- @
--  \f n x -> (mapM_ print . take n . iterate f) x >> return (nest f n x)
-- @
printEveryN :: Show b => (b -> b) -> Int -> b -> IO b
printEveryN = sideEffectEveryN print


-- | Nest the function the given number of times, on the given value, resulting in the given side-effect every iteration
--
-- @
--  \s f n x -> (mapM_ s . take n . iterate f) x >> return (nest f n x)
-- @
sideEffectEveryN :: Monad m => (b -> m a) -> (b -> b) -> Int -> b -> m b
sideEffectEveryN s f = switchEveryN f ((liftM2 (>>) s return) . f)








-- | Seconds taken to be in @[1..59]@. This returns fElse of the input if the clock time's minutes == minutes, otherwise @f x@.
-- Note that this function results in an infinite loop (which is why it has a return type of `Void`).
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


-- | The next step, `switchEvery''` forever
-- @
--  `nestMForever` switchEvery''
-- @
switchEvery' :: (UTCTime, Integer, Int, IO t5 -> IO (IO t5), t5 -> IO (IO t5), IO t5) -> IO (UTCTime, Integer, Int, IO t5 -> IO (IO t5), t5 -> IO (IO t5), IO t5)
switchEvery' = nestMForever switchEvery''




