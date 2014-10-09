
module TestUtil(test, erroneous) where

import Test.QuickCheck
import Test.QuickCheck.Test hiding (test)
import Control.Monad
import Control.Exception.Extra
import Data.Either.Extra
import System.IO.Unsafe


test :: Testable prop => String -> prop -> IO ()
test msg prop = do
    putStrLn msg
    r <- quickCheckResult prop
    unless (isSuccess r) $ error "Test failed"

erroneous :: a -> Bool
erroneous x = unsafePerformIO $ fmap isLeft $ try_ $ evaluate x
