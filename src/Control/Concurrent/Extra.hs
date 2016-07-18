{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "Control.Concurrent".
--
--   This module includes three new types of 'MVar', namely 'Lock' (no associated value),
--   'Var' (never empty) and 'Barrier' (filled at most once). See
--   <http://neilmitchell.blogspot.co.uk/2012/06/flavours-of-mvar_04.html this blog post>
--   for examples and justification.
--
--   If you need greater control of exceptions and threads
--   see the <http://hackage.haskell.org/package/slave-thread slave-thread> package.
--   If you need elaborate relationships between threads
--   see the <http://hackage.haskell.org/package/async async> package.
module Control.Concurrent.Extra(
    module Control.Concurrent,
    getNumCapabilities, setNumCapabilities, withNumCapabilities,
    forkFinally, once, onceFork,
    -- * Lock
    Lock, newLock, withLock, withLockTry,
    -- * Var
    Var, newVar, readVar, writeVar, modifyVar, modifyVar_, withVar,
    -- * Barrier
    Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    ) where

import Control.Concurrent
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Maybe


-- | On GHC 7.6 and above with the @-threaded@ flag, brackets a call to 'setNumCapabilities'.
--   On lower versions (which lack 'setNumCapabilities') this function just runs the argument action.
withNumCapabilities :: Int -> IO a -> IO a
withNumCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act
withNumCapabilities _ act = act


#if __GLASGOW_HASKELL__ < 702
-- | A version of 'getNumCapabilities' that works on all versions of GHC, but returns 1 before GHC 7.2.
getNumCapabilities :: IO Int
getNumCapabilities = return 1
#endif

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


-- | Given an action, produce a wrapped action that runs at most once.
--   If the function raises an exception, the same exception will be reraised each time.
--
-- > let x ||| y = do t1 <- onceFork x; t2 <- onceFork y; t1; t2
-- > \(x :: IO Int) -> void (once x) == return ()
-- > \(x :: IO Int) -> join (once x) == x
-- > \(x :: IO Int) -> (do y <- once x; y; y) == x
-- > \(x :: IO Int) -> (do y <- once x; y ||| y) == x
once :: IO a -> IO (IO a)
once act = do
    var <- newVar OncePending
    let run = either throwIO return
    return $ mask $ \unmask -> join $ modifyVar var $ \v -> case v of
        OnceDone x -> return (v, unmask $ run x)
        OnceRunning x -> return (v, unmask $ run =<< waitBarrier x)
        OncePending -> do
            b <- newBarrier
            return $ (OnceRunning b,) $ do
                res <- try_ $ unmask act
                signalBarrier b res
                modifyVar_ var $ \_ -> return $ OnceDone res
                run res

data Once a = OncePending | OnceRunning (Barrier a) | OnceDone a


-- | Like 'once', but immediately starts running the computation on a background thread.
--
-- > \(x :: IO Int) -> join (onceFork x) == x
-- > \(x :: IO Int) -> (do a <- onceFork x; a; a) == x
onceFork :: IO a -> IO (IO a)
onceFork act = do
    bar <- newBarrier
    forkFinally act $ signalBarrier bar
    return $ either throwIO return =<< waitBarrier bar


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

-- | Create a new 'Lock'.
newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

-- | Perform some operation while holding 'Lock'. Will prevent all other
--   operations from using the 'Lock' while the action is ongoing.
withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const

-- | Like 'withLock' but will never block. If the operation cannot be executed
--   immediately it will return 'Nothing'.
withLockTry :: Lock -> IO a -> IO (Maybe a)
withLockTry (Lock m) act = bracket
    (tryTakeMVar m)
    (\v -> when (isJust v) $ putMVar m ())
    (\v -> if isJust v then fmap Just act else return Nothing)


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

-- | Write a value to become the new value of 'Var'.
writeVar :: Var a -> a -> IO ()
writeVar v x = modifyVar_ v $ const $ return x

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
-- print =<< 'waitBarrier' bar
-- @
--
--   Here we create a barrier which will contain some computed value.
--   A thread is forked to fill the barrier, while the main thread waits
--   for it to complete. A barrier has similarities to a future or promise
--   from other languages, has been known as an IVar in other Haskell work,
--   and in some ways is like a manually managed thunk.
newtype Barrier a = Barrier (Var (Either (MVar ()) a))
    -- Either a Left empty MVar you should wait or a Right result
    -- With base 4.7 and above readMVar is atomic so you probably can implement Barrier directly on MVar a

-- | Create a new 'Barrier'.
newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier $ newVar . Left =<< newEmptyMVar

-- | Write a value into the Barrier, releasing anyone at 'waitBarrier'.
--   Any subsequent attempts to signal the 'Barrier' will throw an exception.
signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier var) v = mask_ $ do -- use mask so never in an inconsistent state
    join $ modifyVar var $ \x -> case x of
        Left bar -> return (Right v, putMVar bar ())
        Right res -> error "Control.Concurrent.Extra.signalBarrier, attempt to signal a barrier that has already been signaled"


-- | Wait until a barrier has been signaled with 'signalBarrier'.
waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier var) = do
    x <- readVar var
    case x of
        Right res -> return res
        Left bar -> do
            readMVar bar
            x <- readVar var
            case x of
                Right res -> return res
                Left bar -> error "Cortex.Concurrent.Extra, internal invariant violated in Barrier"


-- | A version of 'waitBarrier' that never blocks, returning 'Nothing'
--   if the barrier has not yet been signaled.
waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier bar) = fmap (either (const Nothing) Just) $ readVar bar
