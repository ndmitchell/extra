
module TestCustom(testCustom) where

import Control.Concurrent.Extra
import Control.Monad
import System.IO.Extra
import Data.IORef
import TestUtil


testCustom :: IO ()
testCustom = do
    testRaw "withTempFile" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempFile (const $ return ())
            putChar '.'
        sequence_ xs
        putStrLn "done"

    testRaw "withTempDir" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempDir (const $ return ())
            putChar '.'
        sequence_ xs
        putStrLn "done"

    testGen "retry" $ do
        ref <- newIORef 2
        retry 5 $ do modifyIORef ref pred; whenM ((/=) 0 <$> readIORef ref) $ fail "die"
        (==== 0) <$> readIORef ref
