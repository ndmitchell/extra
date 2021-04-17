{-# LANGUAGE TupleSections, ConstraintKinds #-}

-- | Extra functions for "Control.Concurrent".
--
--   This module includes three new types of 'MVar', namely 'Lock' (no associated value),
--   'Var' (never empty) and 'Barrier' (filled at most once). See
--   <https://neilmitchell.blogspot.co.uk/2012/06/flavours-of-mvar_04.html this blog post>
--   for examples and justification.
--
--   If you need greater control of exceptions and threads
--   see the <https://hackage.haskell.org/package/slave-thread slave-thread> package.
--   If you need elaborate relationships between threads
--   see the <https://hackage.haskell.org/package/async async> package.
module Control.Concurrent.Extra(
    module Control.Concurrent,
    withNumCapabilities,
    once, onceFork,
    -- * Lock
    Lock, newLock, withLock, withLockTry,
    -- * Var
    Var, newVar, readVar,
    writeVar, writeVar',
    modifyVar, modifyVar',
    modifyVar_, modifyVar_',
    withVar,
    -- * Barrier
    Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    ) where

import Control.Concurrent
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.Either.Extra
import Data.Functor
import Prelude
import Data.Tuple.Extra (dupe)


-- | On GHC 7.6 and above with the @-threaded@ flag, brackets a call to 'setNumCapabilities'.
--   On lower versions (which lack 'setNumCapabilities') this function just runs the argument action.
withNumCapabilities :: Int -> IO a -> IO a
withNumCapabilities new act | rtsSupportsBoundThreads = do
    old <- getNumCapabilities
    if old == new then act else
        bracket_ (setNumCapabilities new) (setNumCapabilities old) act
withNumCapabilities _ act = act


-- | Given an action, produce a wrapped action that runs at most once.
--   If the function raises an exception, the same exception will be reraised each time.
--
-- > let x ||| y = do t1 <- onceFork x; t2 <- onceFork y; t1; t2
-- > \(x :: IO Int) -> void (once x) == pure ()
-- > \(x :: IO Int) -> join (once x) == x
-- > \(x :: IO Int) -> (do y <- once x; y; y) == x
-- > \(x :: IO Int) -> (do y <- once x; y ||| y) == x
once :: IO a -> IO (IO a)
once act = do
    var <- newVar OncePending
    let run = either throwIO pure
    pure $ mask $ \unmask -> join $ modifyVar var $ \v -> case v of
        OnceDone x -> pure (v, unmask $ run x)
        OnceRunning x -> pure (v, unmask $ run =<< waitBarrier x)
        OncePending -> do
            b <- newBarrier
            pure $ (OnceRunning b,) $ do
                res <- try_ $ unmask act
                signalBarrier b res
                modifyVar_ var $ \_ -> pure $ OnceDone res
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
    pure $ eitherM throwIO pure $ waitBarrier bar


---------------------------------------------------------------------
-- LOCK

-- | Like an 'MVar', but has no value.
--   Used to guarantee single-threaded access, typically to some system resource.
--   As an example:
--
-- @
-- lock <- 'newLock'
-- let output = 'withLock' lock . putStrLn
-- forkIO $ do ...; output \"hello\"
-- forkIO $ do ...; output \"world\"
-- @
--
--   Here we are creating a lock to ensure that when writing output our messages
--   do not get interleaved. This use of 'MVar' never blocks on a put. It is permissible,
--   but rare, that a withLock contains a withLock inside it - but if so,
--   watch out for deadlocks.

newtype Lock = Lock (MVar ())

-- | Create a new 'Lock'.
newLock :: IO Lock
newLock = Lock <$> newMVar ()

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
    (\v -> if isJust v then fmap Just act else pure Nothing)


---------------------------------------------------------------------
-- VAR

-- | Like an 'MVar', but must always be full.
--   Used to operate on a mutable variable in a thread-safe way.
--   As an example:
--
-- @
-- hits <- 'newVar' 0
-- forkIO $ do ...; 'modifyVar_' hits (+1); ...
-- i <- 'readVar' hits
-- print (\"HITS\",i)
-- @
--
--   Here we have a variable which we modify atomically, so modifications are
--   not interleaved. This use of 'MVar' never blocks on a put. No modifyVar
--   operation should ever block, and they should always complete in a reasonable
--   timeframe. A 'Var' should not be used to protect some external resource, only
--   the variable contained within. Information from a 'readVar' should not be subsequently
--   inserted back into the 'Var'.
newtype Var a = Var (MVar a)

-- | Create a new 'Var' with a value.
newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

-- | Read the current value of the 'Var'.
readVar :: Var a -> IO a
readVar (Var x) = readMVar x

-- | Write a value to become the new value of 'Var'.
writeVar :: Var a -> a -> IO ()
writeVar v x = modifyVar_ v $ const $ pure x

-- | Strict variant of 'writeVar'
writeVar' :: Var a -> a -> IO ()
writeVar' v x = modifyVar_' v $ const $ pure x

-- | Modify a 'Var' producing a new value and a return result.
modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

-- | Strict variant of 'modifyVar''
modifyVar' :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar' x f = do
    (newContents, res) <- modifyVar x $ \v -> do
        (newContents, res) <- f v
        pure (newContents, (newContents, res))
    evaluate newContents
    pure res

-- | Modify a 'Var', a restricted version of 'modifyVar'.
modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

-- | Strict variant of 'modifyVar_'
modifyVar_' :: Var a -> (a -> IO a) -> IO ()
modifyVar_' x f = do
    newContents <- modifyVar x (fmap dupe . f)
    _ <- evaluate newContents
    pure ()

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
newtype Barrier a = Barrier (MVar a)

-- | Create a new 'Barrier'.
newBarrier :: IO (Barrier a)
newBarrier = Barrier <$> newEmptyMVar

-- | Write a value into the Barrier, releasing anyone at 'waitBarrier'.
--   Any subsequent attempts to signal the 'Barrier' will throw an exception.
signalBarrier :: Partial => Barrier a -> a -> IO ()
signalBarrier (Barrier var) v = do
    b <- tryPutMVar var v
    unless b $ errorIO "Control.Concurrent.Extra.signalBarrier, attempt to signal a barrier that has already been signaled"


-- | Wait until a barrier has been signaled with 'signalBarrier'.
waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier var) = readMVar var


-- | A version of 'waitBarrier' that never blocks, returning 'Nothing'
--   if the barrier has not yet been signaled.
waitBarrierMaybe :: Barrier a -> IO (Maybe a)
waitBarrierMaybe (Barrier bar) = tryReadMVar bar
