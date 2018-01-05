{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Expire
  (
      Expire(..),
      ExpireIO(..),
      getExpireIO,
      dumpExpireIO,
      killExpireIO,
      ExpireE(..)
  ) where

import Control.Monad
import Data.IORef
import System.Mem.Weak
import Unsafe.Coerce


-- | `expireSteps` is a global number of steps taken for expiration.
-- The idea is that the monad will throw some sort of exception, or result in `Nothing`,
-- when more than this many `Functor`, `Applicative`, or `Monad` steps (actions) have been taken.
expireSteps :: Int
expireSteps = 1


-- | Pure expiration, return `Nothing` to expire
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

-- | `Expire` that throws and `error` instead of returning `Nothing`
newtype ExpireE a = ExpireE { getExpireE :: (Int, a) } deriving (Eq, Ord, Show, Functor)

instance Applicative ExpireE where
  pure = ExpireE . (expireSteps,)

  (~(ExpireE (fi, f))) <*> (~(ExpireE (xi, x))) = (\y -> ExpireE $! if i <= 0 then (0, error "ExpireE: ran out (<*>)") else (i - 1, y)) $! f x
    where
      i = min fi xi

instance Monad ExpireE where
  return = pure

  (~(ExpireE (i, x))) >>= f = (((<*>) (ExpireE (i, flip const x))) . f) $! x


-- | `Expire` with `IO`, use a `Weak` reference outside of the `Maybe` that `Expire` uses.
--
-- Hopefully, `ExpireIO` will give near-instant expiration, not like `Expire`, which seems to take around 1ns per action after expiration.
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



