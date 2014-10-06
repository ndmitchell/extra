{-# LANGUAGE ScopedTypeVariables, CPP #-}

module System.Environment.Extra(module System.Environment, module System.Environment.Extra) where

import Control.Exception as E
import System.IO.Error
import System.Environment


getExePath :: IO FilePath
#if __GLASGOW_HASKELL__ >= 706
getExePath = getExecutablePath
#else
getExePath = getProgName
#endif

---------------------------------------------------------------------
-- System.IO

getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe x = catchJust (\x -> if isDoesNotExistError x then Just x else Nothing) (fmap Just $ getEnv x) (const $ return Nothing)


getEnvVar :: String -> IO (Maybe String)
getEnvVar x = E.catch (fmap Just $ getEnv x) (\(x :: E.SomeException) -> return Nothing)
