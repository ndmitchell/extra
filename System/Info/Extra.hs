{-# LANGUAGE CPP #-}

module System.Info.Extra(
    module System.Info,
    isWindows, getProcessorCount
    ) where

import System.Info
import System.IO.Unsafe
import Control.Exception.Extra
import System.Environment.Extra
import Foreign.C.Types
import Control.Concurrent
import Data.List

---------------------------------------------------------------------
-- System.Info

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif


-- Use the underlying GHC function
foreign import ccall getNumberOfProcessors :: IO CInt


{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
            if rtsSupportsBoundThreads then
                fmap fromIntegral getNumberOfProcessors
            else
                handle_ (const $ return 1) $ do
                    env <- lookupEnv "NUMBER_OF_PROCESSORS"
                    case env of
                        Just s | [(i,"")] <- reads s -> return i
                        _ -> do
                            src <- readFile "/proc/cpuinfo"
                            return $ length [() | x <- lines src, "processor" `isPrefixOf` x]

