-- | This module documents all the functions available in this package.
--
--   Most users should import the specific modules (e.g. @"Data.List.Extra"@), which
--   also reexport their non-@Extra@ modules (e.g. @"Data.List"@).
module Extra(
    -- * Control.Concurrent.Extra
    -- | Extra functions available in @"Control.Concurrent.Extra"@.
    getNumCapabilities, setNumCapabilities, withNumCapabilities, forkFinally, once, Lock, newLock, withLock, withLockTry, Var, newVar, readVar, modifyVar, modifyVar_, withVar, Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    -- * Control.Exception.Extra
    -- | Extra functions available in @"Control.Exception.Extra"@.
    retry, showException, stringException, ignore, catch_, handle_, try_, catchJust_, handleJust_, tryJust_, catchBool, handleBool, tryBool,
    -- * Control.Monad.Extra
    -- | Extra functions available in @"Control.Monad.Extra"@.
    whenJust, unit, loopM, whileM, partitionM, concatMapM, mapMaybeM, findM, firstJustM, whenM, unlessM, ifM, notM, (||^), (&&^), orM, andM, anyM, allM,
    -- * Data.Either.Extra
    -- | Extra functions available in @"Data.Either.Extra"@.
    isLeft, isRight, fromLeft, fromRight, fromEither,
    -- * Data.IORef.Extra
    -- | Extra functions available in @"Data.IORef.Extra"@.
    modifyIORef', writeIORef', atomicModifyIORef', atomicWriteIORef, atomicWriteIORef',
    -- * Data.List.Extra
    -- | Extra functions available in @"Data.List.Extra"@.
    lower, upper, trim, trimStart, trimEnd, word1, dropEnd, takeEnd, splitAtEnd, breakEnd, spanEnd, dropWhileEnd, dropWhileEnd', takeWhileEnd, stripSuffix, wordsBy, linesBy, breakOn, breakOnEnd, splitOn, split, chunksOf, list, uncons, unsnoc, cons, snoc, drop1, groupSort, groupSortOn, groupSortBy, nubOn, groupOn, sortOn, disjoint, allSame, anySame, repeatedly, for, firstJust, concatUnzip, concatUnzip3, replace, merge, mergeBy,
    -- * Data.Tuple.Extra
    -- | Extra functions available in @"Data.Tuple.Extra"@.
    first, second, (***), (&&&), dupe, both, fst3, snd3, thd3,
    -- * Numeric.Extra
    -- | Extra functions available in @"Numeric.Extra"@.
    showDP, intToDouble, intToFloat, floatToDouble, doubleToFloat,
    -- * System.Directory.Extra
    -- | Extra functions available in @"System.Directory.Extra"@.
    withCurrentDirectory, createDirectoryPrivate, listContents, listFiles, listFilesInside, listFilesRecursive,
    -- * System.Environment.Extra
    -- | Extra functions available in @"System.Environment.Extra"@.
    getExecutablePath, lookupEnv,
    -- * System.Info.Extra
    -- | Extra functions available in @"System.Info.Extra"@.
    isWindows,
    -- * System.IO.Extra
    -- | Extra functions available in @"System.IO.Extra"@.
    captureOutput, withBuffering, readFileEncoding, readFileUTF8, readFileBinary, readFile', readFileEncoding', readFileUTF8', readFileBinary', writeFileEncoding, writeFileUTF8, writeFileBinary, withTempFile, withTempDir, newTempFile, newTempDir,
    -- * System.Process.Extra
    -- | Extra functions available in @"System.Process.Extra"@.
    system_, systemOutput, systemOutput_,
    -- * System.Time.Extra
    -- | Extra functions available in @"System.Time.Extra"@.
    Seconds, sleep, timeout, subtractTime, showDuration, offsetTime, offsetTimeIncrease, duration,
    ) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Either.Extra
import Data.IORef.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Numeric.Extra
import System.Directory.Extra
import System.Environment.Extra
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra
import System.Time.Extra
