{-# LANGUAGE ScopedTypeVariables #-}

module TestCustom(testCustom) where

import Control.Concurrent.Extra
import Control.Monad
import System.IO.Extra
import Data.IORef
import Data.Ord
import TestUtil
import Data.List.Extra as X
import Data.Foldable.Extra


testCustom :: IO ()
testCustom = do
    -- check that Extra really does export these things
    testGen "Extra export" $ X.sort [1] == [1]

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

    testGen "`compareLength` works like `compare . length`" $ \(xs :: [Int]) n -> compareLength xs n == compare (length xs) n
    testGen "`compareLength` is lazy" $ compareLength (1:2:3:undefined) 2 == GT
    testGen "`comparingLength` works like `comparing length`" $ \(xs :: [Int]) (ys :: [Int]) -> comparingLength xs ys == comparing length xs ys
    testGen "`comparingLength` is lazy 1/2" $ comparingLength [1,2] (1:2:3:undefined) == LT
    testGen "`comparingLength` is lazy 2/2" $ comparingLength (1:2:3:undefined) [1,2] == GT
