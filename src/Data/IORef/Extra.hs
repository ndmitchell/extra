{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Data.IORef.Extra(
    module Data.IORef,
    modifyIORef', writeIORef', atomicModifyIORef', atomicWriteIORef, atomicWriteIORef'
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


#if __GLASGOW_HASKELL__ < 706

-- | Version of 'modifyIORef' that evaluates the function.
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    writeIORef' ref $ f x

atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()

#endif
