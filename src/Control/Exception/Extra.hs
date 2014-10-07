
module Control.Exception.Extra(
    module Control.Exception,
    retry, showException, ignore,
    catch_, handle_, try_,
    catchJust_, handleJust_, tryJust_,
    catchBool, handleBool, tryBool
    ) where

import Control.Exception
import Control.Monad


-- | Print an exception, but if that exception itself contains exceptions, simply print
--   @\<NestedException\>@. Since Haskell is a lazy language it is possible to throw
--   exceptions that are themselves undefined. This function is useful to report them to users.
showException :: SomeException -> IO String
showException = f . show
    where
        f xs = do
            r <- try_ $ evaluate xs
            case r of
                Left e -> return "<NestedException>"
                Right [] -> return []
                Right (x:xs) -> fmap (x :) $ f xs


-- | Ignore any exceptions thrown by the action and continue as normal.
ignore :: IO () -> IO ()
ignore = void . try_


retry :: Int -> IO a -> IO a
retry i x | i <= 0 = error "retry count must be 1 or more"
retry 1 x = x
retry i x = do
    res <- try_ x
    case res of
        Left _ -> retry (i-1) x
        Right v -> return v


catch_ :: IO a -> (SomeException -> IO a) -> IO a
catch_ = Control.Exception.catch

catchJust_ :: (SomeException -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust_ = catchJust

handle_ :: (SomeException -> IO a) -> IO a -> IO a
handle_ = handle

handleJust_ :: (SomeException -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust_ = handleJust

try_ :: IO a -> IO (Either SomeException a)
try_ = try

tryJust_ :: (SomeException -> Maybe b) -> IO a -> IO (Either b a)
tryJust_ = tryJust

catchBool :: Exception e => (e -> Bool) -> IO a -> (e -> IO a) -> IO a
catchBool f a b = catchJust (bool f) a b

handleBool :: Exception e => (e -> Bool) -> (e -> IO a) -> IO a -> IO a
handleBool f a b = handleJust (bool f) a b

tryBool :: Exception e => (e -> Bool) -> IO a -> IO (Either e a)
tryBool f a = tryJust (bool f) a

bool :: Exception e => (e -> Bool) -> (e -> Maybe e)
bool f x = if f x then Just x else Nothing
