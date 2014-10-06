{-# LANGUAGE CPP #-}

module Control.Concurrent.Extra(module Control.Concurrent.Extra) where

import Control.Concurrent
import Control.Exception


withCapabilities :: Int -> IO a -> IO a
#if __GLASGOW_HASKELL__ >= 706
withCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act
#endif
withCapabilities new act = act

