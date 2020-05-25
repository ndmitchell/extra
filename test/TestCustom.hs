
module TestCustom(testCustom) where

import Control.Concurrent.Extra
import Control.Monad
import System.IO.Extra
import Data.IORef
import TestUtil
import Data.Typeable.Extra as X


testCustom :: IO ()
testCustom = do
    -- check that Extra really does export these things
    testGen "Extra export" $ Proxy == (X.Proxy :: Proxy ())

    testRaw "withTempFile" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempFile (const $ pure ())
            putChar '.'
        sequence_ xs
        putStrLn "done"

    testRaw "withTempDir" $ do
        xs <- replicateM 4 $ onceFork $ do
            replicateM_ 100 $ withTempDir (const $ pure ())
            putChar '.'
        sequence_ xs
        putStrLn "done"

    testGen "retry" $ do
        ref <- newIORef 2
        retry 5 $ do modifyIORef ref pred; whenM ((/=) 0 <$> readIORef ref) $ fail "die"
        (==== 0) <$> readIORef ref

    testRaw "barrier" $ do
        bar <- newBarrier
        (==== Nothing) <$> waitBarrierMaybe bar
        signalBarrier bar 1
        (==== Just 1) <$> waitBarrierMaybe bar
        (==== 1) <$> waitBarrier bar
        Left _ <- try_ $ signalBarrier bar 2
        pure ()
