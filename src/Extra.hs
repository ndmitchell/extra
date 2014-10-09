-- | This module documents all the functions available in this package.
--
--   Most users should import the specific modules (e.g. @"Data.List.Extra"@), which
--   also reexport their non-@Extra@ modules (e.g. @"Data.List"@).
module Extra(
    -- * Control.Concurrent.Extra
    -- | Extra functions available in @"Control.Concurrent.Extra"@.
    withNumCapabilities, setNumCapabilities,
    -- * Control.Exception.Extra
    -- | Extra functions available in @"Control.Exception.Extra"@.
    retry, showException, ignore, catch_, handle_, try_, catchJust_, handleJust_, tryJust_, catchBool, handleBool, tryBool,
    -- * Control.Monad.Extra
    -- | Extra functions available in @"Control.Monad.Extra"@.
    whenJust, unit, partitionM, concatMapM, loopM, whileM, whenM, unlessM, ifM, notM, (||^), (&&^), orM, andM, anyM, allM, findM, firstJustM,
    -- * Data.Either.Extra
    -- | Extra functions available in @"Data.Either.Extra"@.
    isLeft, isRight, fromLeft, fromRight, fromEither,
    -- * Data.IORef.Extra
    -- | Extra functions available in @"Data.IORef.Extra"@.
    modifyIORef', writeIORef', atomicModifyIORef', atomicWriteIORef, atomicWriteIORef',
    -- * Data.List.Extra
    -- | Extra functions available in @"Data.List.Extra"@.
    lower, upper, trim, trimStart, trimEnd, dropAround, word1, drop1, list, uncons, unsnoc, cons, snoc, groupSort, groupSortOn, nubOn, groupOn, sortOn, repeatedly, for, rep, reps, disjoint, allSame, anySame, dropEnd, takeEnd, breakEnd, spanEnd, dropWhileEnd, takeWhileEnd, stripSuffix, concatUnzip, concatUnzip3, merge, mergeBy, replace, wordsBy, linesBy, firstJust, breakOn, breakOnEnd, splitOn, split, chunksOf,
    -- * Data.Tuple.Extra
    -- | Extra functions available in @"Data.Tuple.Extra"@.
    first, second, (***), (&&&), dupe, both, fst3, snd3, thd3, first3, second3, third3, dupe3, both3,
    -- * Numeric.Extra
    -- | Extra functions available in @"Numeric.Extra"@.
    showDP, intToDouble, intToFloat, floatToDouble, doubleToFloat,
    -- * System.Directory.Extra
    -- | Extra functions available in @"System.Directory.Extra"@.
    withCurrentDirectory, getDirectoryContentsRecursive, createDirectoryPrivate,
    -- * System.Environment.Extra
    -- | Extra functions available in @"System.Environment.Extra"@.
    getExecutablePath, lookupEnv,
    -- * System.Info.Extra
    -- | Extra functions available in @"System.Info.Extra"@.
    isWindows, getProcessorCount,
    -- * System.IO.Extra
    -- | Extra functions available in @"System.IO.Extra"@.
    readFileEncoding, readFileUTF8, readFileBinary, readFile', readFileEncoding', readFileUTF8', readFileBinary', writeFileEncoding, writeFileUTF8, writeFileBinary, withTempFile, withTempDir, newTempFile, newTempDir, captureOutput, withBuffering,
    -- * System.Time.Extra
    -- | Extra functions available in @"System.Time.Extra"@.
    sleep, subtractTime, showTime, offsetTime, offsetTimeIncrease, duration,
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
import System.Time.Extra
