
module TestCustom(testCustom) where

import Control.Concurrent.Extra
import Control.Monad
import System.IO.Extra
import TestUtil


testCustom :: IO ()
testCustom = do
    testRaw "withTempFile" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempFile (const $ return ())
        sequence_ xs

    testRaw "withTempDir" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempDir (const $ return ())
        sequence_ xs
