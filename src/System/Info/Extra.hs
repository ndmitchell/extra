{-# LANGUAGE CPP #-}

-- | Extra functions for the current system info.
module System.Info.Extra(
    module System.Info,
    isWindows,
    isMac,
    ) where

import System.Info

---------------------------------------------------------------------
-- System.Info

-- | Return 'True' on Windows and 'False' otherwise. A runtime version of @#ifdef minw32_HOST_OS@.
--   Equivalent to @os == \"mingw32\"@, but: more efficient; doesn't require typing an easily
--   mistypeable string; actually asks about your OS not a library; doesn't bake in
--   32bit assumptions that are already false. \<\/rant\>
--
-- > isWindows == (os == "mingw32")
isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

-- | Return 'True' on Mac OS X and 'False' otherwise.
isMac :: Bool
#if defined(darwin_HOST_OS)
isMac = True
#else
isMac = False
#endif
