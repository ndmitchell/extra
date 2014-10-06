{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module System.Environment.Extra(
    module System.Environment,
    getExecutablePath, lookupEnv
    ) where

import System.Environment

#if __GLASGOW_HASKELL__ < 706
import Control.Exception as E
import System.IO.Error

getExecutablePath :: IO FilePath
getExecutablePath = getProgName

lookupEnv :: String -> IO (Maybe String) Source
lookupEnv x = catchBool isDoesNotExistError (fmap Just $ getEnv x) (const $ return Nothing)
#endif
