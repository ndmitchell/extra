
-- | This module extends "Data.IORef" with operations forcing the value written to the IORef.
--   Some of these functions are available in later versions of GHC, but not all.
module Data.IORef.Extra(
    module Data.IORef,
    writeIORef', atomicWriteIORef'
    ) where

import Data.IORef
import Control.Exception


-- | Evaluates the value before calling 'writeIORef'.
writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = do
    evaluate x
    writeIORef ref x

-- | Evaluates the value before calling 'atomicWriteIORef'.
atomicWriteIORef' :: IORef a -> a -> IO ()
atomicWriteIORef' ref x = do
    evaluate x
    atomicWriteIORef ref x
