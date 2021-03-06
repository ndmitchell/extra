{-# LANGUAGE TypeApplications #-}

module TestCustom (testCustom) where

import Control.Concurrent.Extra
import Control.Monad
import Data.IORef
import Data.List.Extra as X
import System.IO.Extra
import TestUtil

testCustom :: IO ()
testCustom = do
    -- check that Extra really does export these things
    testGen "Extra export" $ X.sort @Int [1] == [1]

    testRaw "withTempFile" $ do
        xs <- replicateM 4 $
            onceFork $ do
                replicateM_ 100 $ withTempFile (const $ pure ())
                putChar '.'
        sequence_ xs
        putStrLn "done"

    testRaw "withTempDir" $ do
        xs <- replicateM 4 $
            onceFork $ do
                replicateM_ 100 $ withTempDir (const $ pure ())
                putChar '.'
        sequence_ xs
        putStrLn "done"

    testGen "retry" $ do
        ref <- newIORef 2
        retry 5 $ do modifyIORef ref (pred @Int); whenM ((/=) 0 <$> readIORef ref) $ fail "die"
        (==== 0) <$> readIORef ref

    testRaw "barrier" $ do
        bar <- newBarrier
        void $ (==== (Nothing :: Maybe Int)) <$> waitBarrierMaybe bar
        signalBarrier bar 1
        void $ (==== Just 1) <$> waitBarrierMaybe bar
        void $ (==== 1) <$> waitBarrier bar
        Left _ <- try_ $ signalBarrier bar 2
        pure ()
