
module TestUtil(runTests, testGen, erroneous, module X) where

import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Exception.Extra
import Data.Either.Extra
import System.IO.Extra
import Data.IORef
import System.IO.Unsafe
import Data.Time.Clock
import Data.Time.Calendar

import Extra as X
import Control.Monad as X
import Data.List as X
import Test.QuickCheck as X


{-# NOINLINE testCount #-}
testCount :: IORef Int
testCount = unsafePerformIO $ newIORef 0

testGen :: Testable prop => String -> prop -> IO ()
testGen msg prop = do
    putStrLn msg
    r <- quickCheckResult prop
    unless (isSuccess r) $ error "Test failed"
    modifyIORef testCount (+1)


erroneous :: a -> Bool
erroneous x = unsafePerformIO $ fmap isLeft $ try_ $ evaluate x


runTests :: IO () -> IO ()
runTests t = do
    writeIORef testCount 0
    t
    n <- readIORef testCount
    putStrLn $ "Success (" ++ show n ++ " tests)"

instance Testable a => Testable (IO a) where
    property = property . unsafePerformIO
    exhaustive = exhaustive . unsafePerformIO

instance Eq a => Eq (IO a) where
    a == b = unsafePerformIO $ do
        a <- try_ $ captureOutput a
        b <- try_ $ captureOutput b
        return $ a == b

instance Eq SomeException where
    a == b = show a == show b

instance Arbitrary UTCTime where
    arbitrary = liftM2 UTCTime arbitrary arbitrary

instance Arbitrary Day where
    arbitrary = fmap ModifiedJulianDay arbitrary

instance Arbitrary DiffTime where
    arbitrary = fmap realToFrac $ choose (0 :: Double, 86401)
