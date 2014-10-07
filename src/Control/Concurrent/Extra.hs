{-# LANGUAGE CPP #-}

module Control.Concurrent.Extra(
    module Control.Concurrent,
    withNumCapabilities, setNumCapabilities
    ) where

import Control.Concurrent
#if __GLASGOW_HASKELL__ >= 706
    hiding (setNumCapabilities)
import qualified Control.Concurrent
#endif
import Control.Exception


-- | On GHC 7.6 and above with the @-threaded@ flag, brackets a call to 'setNumCapabilities'.
--   On lower versions (which lack 'setNumCapabilities') this function just runs the argument action.
withNumCapabilities :: Int -> IO a -> IO a
withNumCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act


-- | A version of 'setNumCapabilities' that works on all versions of GHC, but has no effect before GHC 7.6.
setNumCapabilities :: Int -> IO ()
#if __GLASGOW_HASKELL__ >= 706
setNumCapabilities n = Control.Concurrent.setNumCapabilities n
#else
setNumCapabilities n = return ()
#endif
