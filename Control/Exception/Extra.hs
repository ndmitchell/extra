{-# LANGUAGE ScopedTypeVariables #-}

module Control.Exception.Extra(module Control.Exception.Extra) where

import Control.Exception as E

showException :: SomeException -> IO String
showException = f . show
    where
        f xs = do
            r <- try $ evaluate xs
            case r of
                Left (e :: SomeException) -> return "<NestedException>"
                Right [] -> return []
                Right (x:xs) -> fmap (x :) $ f xs

ignoreExceptions :: IO () -> IO ()
ignoreExceptions act = E.catch act (\(x::SomeException) -> return ())


retry :: Int -> IO a -> IO a
retry i x | i <= 0 = error "retry count must be 1 or more"
retry 1 x = x
retry i x = do
    res <- try x
    case res of
        Left (_ :: SomeException) -> retry (i-1) x
        Right v -> return v


try_ :: IO a -> IO (Either SomeException a)
try_ = try

handle_ :: (SomeException -> IO a) -> IO a -> IO a
handle_ = handle
