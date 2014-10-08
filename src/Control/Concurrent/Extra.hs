{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "Control.Concurrent".
--   The functions manipulate the number of capabilities.
module Control.Concurrent.Extra(
    module Control.Concurrent,
    withNumCapabilities, setNumCapabilities
    ) where

import Control.Concurrent
import Control.Exception


-- | On GHC 7.6 and above with the @-threaded@ flag, brackets a call to 'setNumCapabilities'.
--   On lower versions (which lack 'setNumCapabilities') this function just runs the argument action.
withNumCapabilities :: Int -> IO a -> IO a
withNumCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act


#if __GLASGOW_HASKELL__ < 706
-- | A version of 'setNumCapabilities' that works on all versions of GHC, but has no effect before GHC 7.6.
setNumCapabilities :: Int -> IO ()
setNumCapabilities n = return ()
#endif
