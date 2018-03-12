{-# LANGUAGE ScopedTypeVariables, CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- OK because a test module

module TestUtil
    (runTests
    ,testGen, testRaw
    ,erroneous, erroneousIO
    ,(====), (==>)
    ,ASCIIString(..)
    ,module X
    ) where

import Test.QuickCheck
import Test.QuickCheck.Test
import System.IO.Unsafe
import Text.Show.Functions()

import Control.Applicative as X
import Control.Concurrent.Extra as X
import Control.Exception.Extra as X
import Control.Monad.Extra as X
import Data.Char as X
import Data.Either.Extra as X
import Data.Function as X
import Data.IORef.Extra as X
import Data.List.Extra as X
import Data.Monoid as X
import Data.Tuple.Extra as X
import Data.Typeable.Extra as X
import Data.Version.Extra as X
import Numeric.Extra as X
import System.Directory.Extra as X
import System.FilePath as X
import System.Info.Extra as X
import System.IO.Extra as X
import System.Process.Extra as X
import System.Time.Extra as X


{-# NOINLINE testCount #-}
testCount :: IORef Int
testCount = unsafePerformIO $ newIORef 0

testGen :: Testable prop => String -> prop -> IO ()
testGen msg prop = testRaw msg $ do
    r <- quickCheckResult prop
    unless (isSuccess r) $ error "Test failed"

testRaw :: String -> IO () -> IO ()
testRaw msg test = do
    putStrLn msg
    test
    modifyIORef testCount (+1)


erroneous :: Show a => a -> Bool
erroneous x = unsafePerformIO $ fmap isLeft $ try_ $ evaluate $ length $ show x

erroneousIO :: Show a => IO a -> Bool
erroneousIO x = unsafePerformIO $ fmap isLeft $ try_ $ evaluate . length . show =<< x

(====) :: (Show a, Eq a) => a -> a -> Bool
a ==== b
    | a == b = True
    | otherwise = error $ "Not equal!\n" ++ show a ++ "\n" ++ show b

#if __GLASGOW_HASKELL__ < 707
instance Eq ErrorCall where
    ErrorCall x == ErrorCall y = x == y
#endif

runTests :: IO () -> IO ()
runTests t = do
    writeIORef testCount 0
    t
    n <- readIORef testCount
    putStrLn $ "Success (" ++ show n ++ " tests)"

instance Testable a => Testable (IO a) where
    property = property . unsafePerformIO

-- We only use this property to assert equality as a property
-- And the Show instance is useless (since it may be non-deterministic)
-- So we print out full information on failure
instance (Show a, Eq a) => Eq (IO a) where
    a == b = unsafePerformIO $ do
        a <- try_ $ captureOutput a
        b <- try_ $ captureOutput b
        if a == b then return True else
            error $ show ("IO values not equal", a, b)

instance Show (IO a) where
    show _ = "<<IO>>"

instance Arbitrary a => Arbitrary (IO a) where
    arbitrary = do
        (prnt :: Maybe Int, thrw :: Maybe Int, res) <- arbitrary
        return $ do
            whenJust prnt print
            whenJust thrw (fail . show)
            return res

instance Eq SomeException where
    a == b = show a == show b
