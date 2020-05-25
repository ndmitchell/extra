{-# LANGUAGE DeriveDataTypeable #-}

-- | Extra functions for working with times. Unlike the other modules in this package, there is no
--   corresponding @System.Time@ module. This module enhances the functionality
--   from "Data.Time.Clock", but in quite different ways.
--
--   Throughout, time is measured in 'Seconds', which is a type alias for 'Double'.
module System.Time.Extra(
    Seconds,
    sleep, timeout,
    showDuration,
    offsetTime, offsetTimeIncrease, duration
    ) where

import Control.Concurrent
import System.Clock
import Numeric.Extra
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Typeable
import Data.Unique


-- | A type alias for seconds, which are stored as 'Double'.
type Seconds = Double

-- | Sleep for a number of seconds.
--
-- > fmap (round . fst) (duration $ sleep 1) == pure 1
sleep :: Seconds -> IO ()
sleep = loopM $ \s ->
    -- important to handle both overflow and underflow vs Int
    if s < 0 then
        pure $ Right ()
    else if s > 2000 then do
        threadDelay 2000000000 -- 2000 * 1e6
        pure $ Left $ s - 2000
    else do
        threadDelay $ ceiling $ s * 1000000
        pure $ Right ()


-- An internal type that is thrown as a dynamic exception to
-- interrupt the running IO computation when the timeout has
-- expired.
newtype Timeout = Timeout Unique deriving (Eq,Typeable)
instance Show Timeout where show _ = "<<timeout>>"
instance Exception Timeout


-- | A version of 'System.Timeout.timeout' that takes 'Seconds' and never
--   overflows the bounds of an 'Int'. In addition, the bug that negative
--   timeouts run for ever has been fixed.
--
-- > timeout (-3) (print 1) == pure Nothing
-- > timeout 0.1  (print 1) == fmap Just (print 1)
-- > do (t, _) <- duration $ timeout 0.1 $ sleep 1000; print t; pure $ t < 1
-- > timeout 0.1  (sleep 2 >> print 1) == pure Nothing
timeout :: Seconds -> IO a -> IO (Maybe a)
-- Copied from GHC with a few tweaks.
timeout n f
    | n <= 0 = pure Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleBool (== ex)
                   (const $ pure Nothing)
                   (bracket (forkIOWithUnmask $ \unmask -> unmask $ sleep n >> throwTo pid ex)
                            killThread
                            (\_ -> fmap Just f))


-- | Show a number of seconds, typically a duration, in a suitable manner with
--   reasonable precision for a human.
--
-- > showDuration 3.435   == "3.44s"
-- > showDuration 623.8   == "10m24s"
-- > showDuration 62003.8 == "17h13m"
-- > showDuration 1e8     == "27777h47m"
showDuration :: Seconds -> String
showDuration x
    | x >= 3600 = f (x / 60) "h" "m"
    | x >= 60 = f x "m" "s"
    | otherwise = showDP 2 x ++ "s"
    where
        f x m s = show ms ++ m ++ ['0' | ss < 10] ++ show ss ++ s
            where (ms,ss) = round x `divMod` 60


-- | Call once to start, then call repeatedly to get the elapsed time since the first call.
--   The time is guaranteed to be monotonic. This function is robust to system time changes.
--
-- > do f <- offsetTime; xs <- replicateM 10 f; pure $ xs == sort xs
offsetTime :: IO (IO Seconds)
offsetTime = do
    start <- time
    pure $ do
        end <- time
        pure $ 1e-9 * fromIntegral (toNanoSecs $ end - start)
    where time = getTime Monotonic

{-# DEPRECATED offsetTimeIncrease "Use 'offsetTime' instead, which is guaranteed to always increase." #-}

-- | A synonym for 'offsetTime'.
offsetTimeIncrease :: IO (IO Seconds)
offsetTimeIncrease = offsetTime

-- | Record how long a computation takes in 'Seconds'.
--
-- > do (a,_) <- duration $ sleep 1; pure $ a >= 1 && a <= 1.5
duration :: MonadIO m => m a -> m (Seconds, a)
duration act = do
    time <- liftIO offsetTime
    res <- act
    time <- liftIO time
    pure (time, res)
