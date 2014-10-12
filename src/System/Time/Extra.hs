
module System.Time.Extra(
    Seconds,
    sleep,
    subtractTime,
    showTime,
    offsetTime, offsetTimeIncrease, duration
    ) where

import Control.Concurrent
import Data.Time.Clock
import Numeric.Extra
import Data.IORef

type Seconds = Double

sleep :: Seconds -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000


subtractTime :: UTCTime -> UTCTime -> Seconds
subtractTime end start = fromRational $ toRational $ end `diffUTCTime` start


showTime :: Seconds -> String
showTime x | x >= 3600 = f (x / 60) "h" "m"
           | x >= 60 = f x "m" "s"
           | otherwise = showDP 2 x ++ "s"
    where
        f x m s = show ms ++ m ++ ['0' | ss < 10] ++ show ss ++ m
            where (ms,ss) = round x `divMod` 60



-- | Call once at the start, then call repeatedly to get Time values out
offsetTime :: IO (IO Seconds)
offsetTime = do
    start <- getCurrentTime
    return $ do
        end <- getCurrentTime
        return $ end `subtractTime` start

-- | Like offsetTime, but results will never decrease (though they may stay the same)
offsetTimeIncrease :: IO (IO Seconds)
offsetTimeIncrease = do
    t <- offsetTime
    ref <- newIORef 0
    return $ do
        t <- t
        atomicModifyIORef ref $ \o -> let m = max t o in m `seq` (m, m)


duration :: IO a -> IO (Seconds, a)
duration act = do
    time <- offsetTime
    res <- act
    time <- time
    return (time, res)
