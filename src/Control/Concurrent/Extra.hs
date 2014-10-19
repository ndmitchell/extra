{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "Control.Concurrent". These fall into a few categories:
--
-- * Some functions manipulate the number of capabilities.
--
-- * The 'forkFinally' function - if you need greater control of exceptions and threads
--   see the <http://hackage.haskell.org/package/slave-thread slave-thread> package.
--
-- * Three new types of 'MVar', namely 'Lock' (no associated value), 'Var' (never empty)
--   and 'Barrier' (filled at most once). See
--   <http://neilmitchell.blogspot.co.uk/2012/06/flavours-of-mvar_04.html this blog post>
--   for more examples.
module Control.Concurrent.Extra(
    module Control.Concurrent,
    withNumCapabilities, setNumCapabilities,
    forkFinally,
    -- * Lock
    Lock, newLock, withLock, withLockTry,
    -- * Var
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    -- * Barrier
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
-- @
-- forkFinally action and_then =
--    mask $ \restore ->
--      forkIO $ try (restore action) >>= and_then
-- @
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

-- | Like an MVar, but has no value.
--   Used to guarantees single-threaded access, typically to some system resource. 
--   As an example:
--
-- @
-- lock <- 'newLock'
-- let output = 'withLock' . putStrLn
-- forkIO $ do ...; output \"hello\"
-- forkIO $ do ...; output \"world\"
-- @
--
--   Here we are creating a lock to ensure that when writing output our messages
--   do not get interleaved. This use of MVar never blocks on a put. It is permissible,
--   but rare, that a withLock contains a withLock inside it - but if so,
--   watch out for deadlocks.

newtype Lock = Lock (MVar ())

-- | Create a 'newLock'.
newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

-- | Perform some operation while holding 'Lock'. Will prevent all other
--   operations from using the 'Lock' while the action is ongoing.
withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const

-- | Like 'withLock' but will never block. If the operation cannot be executed
--   immediately it will return 'Nothing'.
withLockTry :: Lock -> IO a -> IO (Maybe a)
withLockTry (Lock m) act =
    mask $ \restore -> do
        a <- tryTakeMVar m
        case a of
            Nothing -> return Nothing
            Just _ -> restore (fmap Just act) `finally` putMVar m ()


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full.
--   Used to on a mutable variable in a thread-safe way.
--   As an example:
--
-- @
-- hits <- 'newVar' 0
-- forkIO $ do ...; 'modifyVar_' hits (+1); ...
-- i <- 'readVar' hits
-- print ("HITS",i)
-- @
--
--   Here we have a variable which we modify atomically, so modifications are
--   not interleaved. This use of MVar never blocks on a put. No modifyVar
--   operation should ever block, and they should always complete in a reasonable
--   timeframe. A Var should not be used to protect some external resource, only
--   the variable contained within. Information from a readVar should not be subsequently
--   inserted back into the Var.
newtype Var a = Var (MVar a)

-- | Create a new 'Var' with a value.
newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

-- | Read the current value of the 'Var'.
readVar :: Var a -> IO a
readVar (Var x) = readMVar x

-- | Modify a 'Var' producing a new value and a return result.
modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

-- | Modify a 'Var', a restricted version of 'modifyVar'.
modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

-- | Perform some operation using the value in the 'Var',
--   a restricted version of 'modifyVar'.
withVar :: Var a -> (a -> IO b) -> IO b
withVar (Var x) f = withMVar x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once. As an example:
--
-- @
-- bar <- 'newBarrier'
-- forkIO $ do ...; val <- ...; 'signalBarrier' bar val
-- print =<< waitBarrier bar
-- @
--
--   Here we create a barrier which will contain some computed value.
--   A thread is forked to fill the barrier, while the main thread waits
--   for it to complete. A barrier has similarities to a future or promise
--   from other languages, has been known as an IVar in other Haskell work,
--   and in some ways is like a manually managed thunk.
newtype Barrier a = Barrier (MVar a)

-- | Create a new 'Barrier'.
newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

-- | Write a value into the Barrier, releasing anyone at 'waitBarrier'.
--   Any subsequent attempts to signal the 'Barrier' will be silently ignored.
signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = void . tryPutMVar x

-- | Wait until a barrier has been signaled with 'signalBarrier'.
waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x

-- | A version of 'waitBarrier' that never blocks, returning 'Nothing'
--   if the barrier has not yet been signaled.
waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier x) = do
    res <- tryTakeMVar x
    whenJust res $ void . tryPutMVar x
    return res
