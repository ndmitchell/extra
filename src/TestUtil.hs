module TestUtil(test) where

import Test.QuickCheck
import Test.QuickCheck.Test hiding (test)
import Control.Monad

test :: Testable prop => String -> prop -> IO ()
test msg prop = do
    putStrLn msg
    r <- quickCheckResult prop
    unless (isSuccess r) $ error "Test failed"
