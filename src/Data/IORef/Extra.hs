
-- | This module extends "Data.IORef" with operations forcing the value written to the IORef.
--   Some of these functions are available in later versions of GHC, but not all.
module Data.IORef.Extra(
    module Data.IORef,
    writeIORef', atomicWriteIORef',
    atomicModifyIORef_, atomicModifyIORef'_
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


-- | Variant of 'atomicModifyIORef' which ignores the return value
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ r f = atomicModifyIORef r $ \v -> (f v, ())

-- | Variant of 'atomicModifyIORef'' which ignores the return value
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ r f = atomicModifyIORef' r $ \v -> (f v, ())
