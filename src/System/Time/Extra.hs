
module System.Time.Extra(
    Seconds,
    sleep,
    subtractTime,
    showDuration,
    offsetTime, offsetTimeIncrease, duration
    ) where

import Control.Concurrent
import Data.Time.Clock
import Numeric.Extra
import Data.IORef

-- | A type alias for seconds, which are stored as 'Double'.
type Seconds = Double

-- | Sleep for a number of seconds.
--
-- > fmap (round . fst) (duration $ sleep 1) == return 1
sleep :: Seconds -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


-- Calculate the difference between two times in seconds.
-- Usually the first time will be the end of an event, and the
-- second time will be the beginning.
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
