
-- | Extra functions for "Control.Exception".
--   These functions provide retrying, showing in the presence of exceptions,
--   and functions to catch\/ignore exceptions, including monomorphic (no 'Exception' context) versions.
module Control.Exception.Extra(
    module Control.Exception,
    retry,
    showException, stringException,
    -- * Exception catching/ignoring
    ignore,
    catch_, handle_, try_,
    catchJust_, handleJust_, tryJust_,
    catchBool, handleBool, tryBool
    ) where

import Control.Exception
import Control.Monad
import Data.List.Extra


-- | Fully evaluate an input String. If the String contains embedded exceptions it will produce @\<Exception\>@.
--
-- > stringException "test"                           == return "test"
-- > stringException ("test" ++ undefined)            == return "test<Exception>"
-- > stringException ("test" ++ undefined ++ "hello") == return "test<Exception>"
-- > stringException ['t','e','s','t',undefined]      == return "test<Exception>"
stringException :: String -> IO String
stringException x = do
    r <- try_ $ evaluate $ list [] (\x xs -> x `seq` x:xs) x
    case r of
        Left e -> return "<Exception>"
        Right [] -> return []
        Right (x:xs) -> fmap (x:) $ stringException xs


-- | Show a value, but if the result contains exceptions, produce
--   @\<Exception\>@. Defined as @'stringException' . show@.
--   Particularly useful for printing exceptions to users, remembering that exceptions
--   can themselves contain undefined values.
showException :: Show e => e -> IO String
showException = stringException . show


-- | Ignore any exceptions thrown by the action.
--
-- > ignore (print 1)    == print 1
-- > ignore (fail "die") == return ()
ignore :: IO () -> IO ()
ignore = void . try_


-- | Retry an operation at most /n/ times (/n/ must be positive).
--   If the operation fails the /n/th time it will throw that final exception.
--
-- > retry 1 (print "x")  == print "x"
-- > retry 3 (fail "die") == fail "die"
retry :: Int -> IO a -> IO a
retry i x | i <= 0 = error "retry count must be 1 or more"
retry 1 x = x
retry i x = do
    res <- try_ x
    case res of
        Left _ -> retry (i-1) x
        Right v -> return v


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
-- readFileExists x == catchBool isDoesNotExistError (readFile \"myfile\") (const $ return \"\")
-- @
catchBool :: Exception e => (e -> Bool) -> IO a -> (e -> IO a) -> IO a
catchBool f a b = catchJust (bool f) a b

-- | Like 'catchBool' but for 'handle'.
handleBool :: Exception e => (e -> Bool) -> (e -> IO a) -> IO a -> IO a
handleBool f a b = handleJust (bool f) a b

-- | Like 'catchBool' but for 'try'.
tryBool :: Exception e => (e -> Bool) -> IO a -> IO (Either e a)
tryBool f a = tryJust (bool f) a

bool :: Exception e => (e -> Bool) -> (e -> Maybe e)
bool f x = if f x then Just x else Nothing
