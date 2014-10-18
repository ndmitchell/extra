{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "Control.Concurrent".
--   These functions manipulate the number of capabilities.
module Control.Concurrent.Extra(
    module Control.Concurrent,
    withNumCapabilities, setNumCapabilities,
    forkFinally,
    Lock, newLock, withLock, withLockTry,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Extra


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


#if __GLASGOW_HASKELL__ < 706
-- | fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
--
-- >> forkFinally action and_then =
-- >>   mask $ \restore ->
-- >>     forkIO $ try (restore action) >>= and_then
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif


---------------------------------------------------------------------
-- LOCK

-- | Like an MVar, but has no value
newtype Lock = Lock (MVar ())
instance Show Lock where show _ = "Lock"

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const

withLockTry :: Lock -> IO a -> IO (Maybe a)
withLockTry (Lock m) act =
    mask $ \restore -> do
        a <- tryTakeMVar m
        case a of
            Nothing -> return Nothing
            Just _ -> restore (fmap Just act) `finally` putMVar m ()


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)
instance Show (Var a) where show _ = "Var"

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

withVar :: Var a -> (a -> IO b) -> IO b
withVar (Var x) f = withMVar x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once
newtype Barrier a = Barrier (MVar a)
instance Show (Barrier a) where show _ = "Barrier"

newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = void . tryPutMVar x

waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x

waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier x) = do
    res <- tryTakeMVar x
    whenJust res $ void . tryPutMVar x
    return res
