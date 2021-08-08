{-# LANGUAGE ScopedTypeVariables, CPP, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for "Control.Exception".
--   These functions provide retrying, showing in the presence of exceptions,
--   and functions to catch\/ignore exceptions, including monomorphic (no 'Exception' context) versions.
--
--   If you want to use a safer set of exceptions see the
--   <https://hackage.haskell.org/package/safe-exceptions safe-exceptions> package.
module Control.Exception.Extra(
    module Control.Exception,
    Partial,
    retry, retryBool,
    errorWithoutStackTrace,
    showException, stringException,
    errorIO, assertIO,
    -- * Exception catching/ignoring
    ignore,
    catch_, handle_, try_,
    catchJust_, handleJust_, tryJust_,
    catchBool, handleBool, tryBool
    ) where

#if __GLASGOW_HASKELL__ >= 800
import GHC.Stack
#endif

import Control.Exception
import Control.Monad
import Data.List.Extra
import Data.Functor
import Partial
import Prelude


-- | Fully evaluate an input String. If the String contains embedded exceptions it will produce @\<Exception\>@.
--
-- > stringException "test"                           == pure "test"
-- > stringException ("test" ++ undefined)            == pure "test<Exception>"
-- > stringException ("test" ++ undefined ++ "hello") == pure "test<Exception>"
-- > stringException ['t','e','s','t',undefined]      == pure "test<Exception>"
stringException :: String -> IO String
stringException x = do
    r <- try_ $ evaluate $ list [] (\x xs -> x `seq` x:xs) x
    case r of
        Left e -> pure "<Exception>"
        Right [] -> pure []
        Right (x:xs) -> (x:) <$> stringException xs


-- | Show a value, but if the result contains exceptions, produce
--   @\<Exception\>@. Defined as @'stringException' . show@.
--   Particularly useful for printing exceptions to users, remembering that exceptions
--   can themselves contain undefined values.
showException :: Show e => e -> IO String
showException = stringException . show


#if __GLASGOW_HASKELL__ < 800
-- | A variant of 'error' that does not produce a stack trace.
errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = error
#endif


-- | Ignore any exceptions thrown by the action.
--
-- > ignore (print 1)    == print 1
-- > ignore (fail "die") == pure ()
ignore :: IO () -> IO ()
ignore = void . try_


-- | An 'IO' action that when evaluated calls 'error', in the 'IO' monad.
--   Note that while 'fail' in 'IO' raises an 'IOException', this function raises an 'ErrorCall' exception with a call stack.
--
-- > catch (errorIO "Hello") (\(ErrorCall x) -> pure x) == pure "Hello"
-- > seq (errorIO "foo") (print 1) == print 1
{-# NOINLINE errorIO #-} -- otherwise GHC 8.4.1 seems to get upset
errorIO :: Partial => String -> IO a
errorIO x = withFrozenCallStack $ evaluate $ error x

#if __GLASGOW_HASKELL__ < 800
withFrozenCallStack :: a -> a
withFrozenCallStack = id
#endif

-- | An 'IO' action that when evaluated calls 'assert' in the 'IO' monad, which throws an 'AssertionFailed' exception if the argument is 'False'.
--   With optimizations enabled (and @-fgnore-asserts@) this function ignores its argument and does nothing.
--
-- > catch (assertIO True  >> pure 1) (\(x :: AssertionFailed) -> pure 2) == pure 1
-- > seq (assertIO False) (print 1) == print 1
assertIO :: Partial => Bool -> IO ()
assertIO x = withFrozenCallStack $ evaluate $ assert x ()

-- | Retry an operation at most /n/ times (/n/ must be positive).
--   If the operation fails the /n/th time it will throw that final exception.
--
-- > retry 1 (print "x")  == print "x"
-- > retry 3 (fail "die") == fail "die"
retry :: Int -> IO a -> IO a
retry i x | i <= 0 = error "Control.Exception.Extra.retry: count must be 1 or more"
retry i x = retryBool (\(e :: SomeException) -> True) i x

-- | Retry an operation at most /n/ times (/n/ must be positive), while the exception value and type match a predicate.
--   If the operation fails the /n/th time it will throw that final exception.
retryBool :: Exception e => (e -> Bool) -> Int -> IO a -> IO a
retryBool p i x | i <= 0 = error "Control.Exception.Extra.retryBool: count must be 1 or more"
retryBool p 1 x = x
retryBool p i x = do
    res <- tryBool p x
    case res of
        Left _ -> retryBool p (i-1) x
        Right v -> pure v


-- | A version of 'catch' without the 'Exception' context, restricted to 'SomeException',
--   so catches all exceptions.
catch_ :: IO a -> (SomeException -> IO a) -> IO a
catch_ = Control.Exception.catch

-- | Like 'catch_' but for 'catchJust'
catchJust_ :: (SomeException -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust_ = catchJust

-- | Like 'catch_' but for 'handle'
handle_ :: (SomeException -> IO a) -> IO a -> IO a
handle_ = handle

-- | Like 'catch_' but for 'handleJust'
handleJust_ :: (SomeException -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust_ = handleJust

-- | Like 'catch_' but for 'try'
try_ :: IO a -> IO (Either SomeException a)
try_ = try

-- | Like 'catch_' but for 'tryJust'
tryJust_ :: (SomeException -> Maybe b) -> IO a -> IO (Either b a)
tryJust_ = tryJust

-- | Catch an exception if the predicate passes, then call the handler with the original exception.
--   As an example:
--
-- @
-- readFileExists x == catchBool isDoesNotExistError (readFile \"myfile\") (const $ pure \"\")
-- @
catchBool :: Exception e => (e -> Bool) -> IO a -> (e -> IO a) -> IO a
catchBool f a b = catchJust (bool f) a b

-- | Like 'catchBool' but for 'handle'.
handleBool :: Exception e => (e -> Bool) -> (e -> IO a) -> IO a -> IO a
handleBool f a b = handleJust (bool f) a b

-- | Like 'catchBool' but for 'try'.
tryBool :: Exception e => (e -> Bool) -> IO a -> IO (Either e a)
tryBool f a = tryJust (bool f) a

bool :: (e -> Bool) -> (e -> Maybe e)
bool f x = if f x then Just x else Nothing
