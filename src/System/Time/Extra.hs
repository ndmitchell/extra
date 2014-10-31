{-# LANGUAGE DeriveDataTypeable #-}

-- | Extra functions for working with times. Unlike the other modules in this package, there is no
--   corresponding @System.Time@ module. This module enhances the functionality
--   from "Data.Time.Clock", but in quite different ways.
--
--   Throughout, time is measured in 'Seconds', which is a type alias for 'Double'.
module System.Time.Extra(
    Seconds,
    sleep, timeout,
    subtractTime,
    showDuration,
    offsetTime, offsetTimeIncrease, duration
    ) where

import Control.Concurrent
import Data.Time.Clock
import Numeric.Extra
import Data.IORef
import Control.Monad.Extra
import Control.Exception.Extra
import Data.Typeable
import Data.Unique


-- | A type alias for seconds, which are stored as 'Double'.
type Seconds = Double

-- | Sleep for a number of seconds.
--
-- > fmap (round . fst) (duration $ sleep 1) == return 1
sleep :: Seconds -> IO ()
sleep = loopM $ \s ->
    -- important to handle both overflow and underflow vs Int
    if s < 0 then
        return $ Right ()
    else if s > 2000 then do
        threadDelay 2000000000 -- 2000 * 1e6
        return $ Left $ s - 2000
    else do
        threadDelay $ ceiling $ s * 1000000
        return $ Right ()


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
-- > timeout (-3) (print 1) == return Nothing
-- > timeout 0.1  (print 1) == fmap Just (print 1)
-- > timeout 0.1  (sleep 2 >> print 1) == return Nothing
-- > do (t, _) <- duration $ timeout 0.1 $ sleep 1000; return $ t < 1
timeout :: Seconds -> IO a -> IO (Maybe a)
-- Copied from GHC with a few tweaks.
timeout n f
    | n <= 0 = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleBool (== ex) 
                   (const $ return Nothing)
                   (bracket (forkIOWithUnmask $ \unmask -> unmask $ sleep n >> throwTo pid ex)
                            (killThread)
                            (\_ -> fmap Just f))


-- | Calculate the difference between two times in seconds.
--   Usually the first time will be the end of an event, and the
--   second time will be the beginning.
--
-- > \a b -> a > b ==> subtractTime a b > 0
subtractTime :: UTCTime -> UTCTime -> Seconds
subtractTime end start = fromRational $ toRational $ end `diffUTCTime` start


-- | Show a number of seconds, typically a duration, in a suitable manner with
--   responable precision for a human.
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


-- | Call once to start, then call repeatedly to get the elapsed time since the first
--   call. Values will usually increase, unless the system clock is updated
--   (if you need the guarantee, see 'offsetTimeIncrease').
offsetTime :: IO (IO Seconds)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ end `subtractTime` start

-- | Like 'offsetTime', but results will never decrease (though they may stay the same).
--
-- > do f <- offsetTimeIncrease; xs <- replicateM 10 f; return $ xs == sort xs
offsetTimeIncrease :: IO (IO Seconds)
offsetTimeIncrease = do
    t <- offsetTime
    ref <- newIORef 0
    return $ do
        t <- t
        atomicModifyIORef ref $ \o -> let m = max t o in m `seq` (m, m)

-- | Record how long a computation takes in 'Seconds'.
duration :: IO a -> IO (Seconds, a)
duration act = do
    time <- offsetTime
    res <- act
    time <- time
    return (time, res)
