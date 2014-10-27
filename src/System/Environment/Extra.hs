{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "System.Environment". All these functions are available in later GHC versions,
--   but this code works all the way back to GHC 7.2.
module System.Environment.Extra(
    module System.Environment,
    getExecutablePath, lookupEnv
    ) where

import System.Environment

#if __GLASGOW_HASKELL__ < 706
import Control.Exception.Extra
import System.IO.Error

getExecutablePath :: IO FilePath
getExecutablePath = getProgName

lookupEnv :: String -> IO (Maybe String)
lookupEnv x = catchBool isDoesNotExistError (fmap Just $ getEnv x) (const $ return Nothing)
#endif
