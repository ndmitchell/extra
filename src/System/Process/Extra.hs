{-# LANGUAGE TupleSections #-}

module System.Process.Extra(
    module System.Process,
    system_, systemOutput, systemOutput_
    ) where

import Control.Monad
import System.IO.Extra
import System.Process
import System.Exit


systemOutput :: String -> IO (ExitCode, String)
systemOutput x = withTempFile $ \file -> do
    exit <- withFile file WriteMode $ \h -> do
        (_, _, _, pid) <- createProcess (shell x){std_out=UseHandle h, std_err=UseHandle h}
        waitForProcess pid
    fmap (exit,) $ readFile' file


system_ :: String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Failed when running system command: " ++ x

systemOutput_ :: String -> IO String
systemOutput_ x = do
    (res,out) <- systemOutput x
    when (res /= ExitSuccess) $
        error $ "Failed when running system command: " ++ x
    return out
