{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

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
      switchEvery'',
      Expire(..),
      ExpireIO(..),
      getExpireIO,
      dumpExpireIO,
      killExpireIO,
      ExpireE(..)
    ) where


import Control.Monad (foldM, liftM, liftM2)
import Data.Time.Clock (UTCTime, getCurrentTime, utctDayTime, diffUTCTime)
import Data.Void

import Data.IORef
import Control.Monad
import System.Mem.Weak
import Unsafe.Coerce
import Control.Applicative

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

-- | `nestByM` generalizes `nestM` to numbers inside monads (for example, @mn@ could be (read.getLine)).
nestByM :: Monad m => (b -> m b) -> b -> m Int -> m b
nestByM f x mn = foldM (\y z -> z f y) x =<< liftM (flip replicate ($)) mn

-- | `nestMForever` is equivalent to `nestM f x Infinity`.
nestMForever :: Monad m => (b -> m b) -> b -> m b
nestMForever f x = foldM ((. const) . (.) $ f) x (repeat id)

expireSteps :: Int
expireSteps = 1

newtype Expire a = Expire { getExpire :: (Int, Maybe a) } deriving (Eq, Ord, Show, Functor)

instance Applicative Expire where
  pure = Expire . (expireSteps,) . Just

  (~(Expire (_, f))) <*> (~(Expire (xi, x))) = maybe (Expire (0, Nothing)) (\y -> Expire $! if xi <= 0 then (0, Nothing) else (xi - 1, Just y)) $! f <*> x
                                         -- then Expire (0, Nothing)
                                         -- else Expire (abs xi - 1, f <*> x)
    -- where
      -- i = xi
  -- Expire (_ , _) <*> Expire (0 , _) = Expire (0, Nothing)
  -- Expire (_ , f) <*> Expire (xi, x) = Expire (abs xi - 1, f <*> x)

instance Monad Expire where
  return = pure

  (~(Expire (i, x))) >>= f = maybe (Expire (0, Nothing)) (((<*>) (Expire (i, flip const <$> x))) . f) x

newtype ExpireE a = ExpireE { getExpireE :: (Int, a) } deriving (Eq, Ord, Show, Functor)

instance Applicative ExpireE where
  pure = ExpireE . (expireSteps,)

  (~(ExpireE (fi, f))) <*> (~(ExpireE (xi, x))) = (\y -> ExpireE $! if i <= 0 then (0, error "ExpireE: ran out (<*>)") else (i - 1, y)) $! f x
    where
      i = min fi xi

instance Monad ExpireE where
  return = pure

  (~(ExpireE (i, x))) >>= f = (((<*>) (ExpireE (i, flip const x))) . f) $! x


newtype ExpireIO a = ExpireIO { runExpireIO :: IO (Weak (IORef (Int, Maybe a))) }

-- mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))


-- | Deref an `ExpireIO`, only returning the value, or return `Nothing`
getExpireIO :: ExpireIO a -> IO (Maybe a)
getExpireIO ~(ExpireIO wref) = do
  wref' <- wref
  ref   <- deRefWeak wref'
  maybe (return Nothing) (fmap snd . readIORef) ref

-- | Dereference an `ExpireIO`, or return `Nothing`
dumpExpireIO :: ExpireIO a -> IO (Maybe (Int, a))
dumpExpireIO ~(ExpireIO wref) = do
  wref' <- wref
  ref   <- deRefWeak wref'
  maybe (return Nothing) ((>>= \(i, x) -> return ((i,) <$> x)) . readIORef) ref

-- | `finalize` an `ExpireIO`
killExpireIO :: ExpireIO a -> IO ()
killExpireIO ~(ExpireIO wref) = do
  wref' <- wref
  finalize wref'


-- | Note: `stepExpire` will never halt if `Int` is negative, so instead it throws an `error`
stepExpire :: Int -> a -> (Int, Maybe a)
stepExpire 0 _             = (0, Nothing)
stepExpire 1 _             = (0, Nothing)
stepExpire i x | i < 0     = error "stepExpire: negative input, i.e. caught infinite loop"
               | otherwise = (i-1, Just x)

-- | `stepExpire` for `Maybe`
stepExpireMaybe :: Int -> Maybe a -> (Int, Maybe a)
stepExpireMaybe _ Nothing   = (0, Nothing)
stepExpireMaybe i ~(Just x) = stepExpire i x

-- | Note: This is only safe if a `Weak` pointer has _already_ been dereferenced
coerceWeak :: Weak a -> Weak b
coerceWeak = unsafeCoerce


instance Functor ExpireIO where
  fmap :: (a -> b) -> ExpireIO a -> ExpireIO b
  fmap f (ExpireIO wref) = ExpireIO $ do
    wref' <- wref
    ref   <- deRefWeak wref'
    flip (maybe (unsafeCoerce wref)) ref $ \ioRef -> do
      (i, x) <- fmap (fmap f) . uncurry stepExpireMaybe <$> readIORef ioRef
      case x of
        Nothing    -> do
          finalize wref'
          fmap unsafeCoerce wref -- deRefWeak should now result in null
        ~(Just x') -> do
          finalize wref'
          ioRef' <- newIORef (i, Just x')
          mkWeakIORef ioRef' (return ())

instance Applicative ExpireIO where
  pure x = ExpireIO $ do
    ioRef <- newIORef (expireSteps, Just x)
    mkWeakIORef ioRef (return ())

  ExpireIO fWref <*> ExpireIO xWref = ExpireIO $ do
    fWref' <- fWref
    xWref' <- xWref
    fRef' <- deRefWeak fWref'
    xRef' <- deRefWeak xWref'
    case liftM2 (,) fRef' xRef' of
      Nothing -> do -- either input was unable to be dereferenced, ensure both are finalized and do nothing
        finalize fWref'
        finalize xWref'
        return $ coerceWeak xWref'
      ~(Just (fIOref, xIOref)) -> do -- both were successfully dereferenced!
        (i, x) <- uncurry stepExpireMaybe <$> liftM2 (\(_, b) (c, d) -> (c, b <*> d)) (readIORef fIOref) (readIORef xIOref)
        case x of
          Nothing -> do -- just ended, finalize both
            finalize fWref'
            finalize xWref'
            return $ coerceWeak xWref'
          ~(Just y) -> do -- make new value, finalizing x
            finalize fWref'
            yRef <- newIORef (i, Just y)
            mkWeakIORef yRef (return ())

instance Monad ExpireIO where
  return = pure

  ExpireIO wref >>= f = ExpireIO $ do
    wref' <- wref
    ref   <- deRefWeak wref'
    case ref of
      Nothing -> do
        finalize wref'
        return $ coerceWeak wref'
      ~(Just ioRef) -> do
        ~(_, x) <- uncurry stepExpireMaybe <$> readIORef ioRef
        case x of
          Nothing -> do
            finalize wref'
            return $ coerceWeak wref'
          ~(Just y) -> do
            finalize wref'
            runExpireIO $ (flip const <$> ExpireIO wref) <*> f y


-- Hopefully, ExpireIO will give near-instant expiration, not like Expire, which seems to take around 1ns per action after expiration.


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

