{-# LANGUAGE TupleSections, ConstraintKinds #-}

-- | Extra functions for creating processes. Specifically variants that automatically check
--   the 'ExitCode' and capture the 'stdout' \/ 'stderr' handles.
module System.Process.Extra(
    module System.Process,
    system_, systemOutput, systemOutput_
    ) where

import Control.Monad
import System.IO.Extra
import System.Process
import System.Exit
import Data.Functor
import Partial
import Prelude


-- | A version of 'system' that also captures the output, both 'stdout' and 'stderr'.
--   Returns a pair of the 'ExitCode' and the output.
systemOutput :: String -> IO (ExitCode, String)
systemOutput x = withTempFile $ \file -> do
    exit <- withFile file WriteMode $ \h -> do
        (_, _, _, pid) <- createProcess (shell x){std_out=UseHandle h, std_err=UseHandle h}
        waitForProcess pid
    (exit,) <$> readFile' file


-- | A version of 'system' that throws an error if the 'ExitCode' is not 'ExitSuccess'.
system_ :: Partial => String -> IO ()
system_ x = do
    res <- system x
    when (res /= ExitSuccess) $
        error $ "Failed when running system command: " ++ x

-- | A version of 'system' that captures the output (both 'stdout' and 'stderr')
--   and throws an error if the 'ExitCode' is not 'ExitSuccess'.
systemOutput_ :: Partial => String -> IO String
systemOutput_ x = do
    (res,out) <- systemOutput x
    when (res /= ExitSuccess) $
        error $ "Failed when running system command: " ++ x
    pure out
