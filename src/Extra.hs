-- GENERATED CODE - DO NOT MODIFY
-- See Generate.hs for details of how to generate

-- | This module documents all the functions available in this package.
--
--   Most users should import the specific modules (e.g. @"Data.List.Extra"@), which
--   also reexport their non-@Extra@ modules (e.g. @"Data.List"@).
module Extra {-# DEPRECATED "This module is provided as documentation of all new functions, you should import the more specific modules directly." #-} (
    -- * Control.Concurrent.Extra
    -- | Extra functions available in @"Control.Concurrent.Extra"@.
    getNumCapabilities, setNumCapabilities, withNumCapabilities, forkFinally, once, onceFork, Lock, newLock, withLock, withLockTry, Var, newVar, readVar, writeVar, modifyVar, modifyVar_, withVar, Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    -- * Control.Exception.Extra
    -- | Extra functions available in @"Control.Exception.Extra"@.
    Partial, retry, retryBool, errorWithoutStackTrace, showException, stringException, errorIO, displayException, ignore, catch_, handle_, try_, catchJust_, handleJust_, tryJust_, catchBool, handleBool, tryBool,
    -- * Control.Monad.Extra
    -- | Extra functions available in @"Control.Monad.Extra"@.
    whenJust, whenJustM, unit, maybeM, eitherM, loopM, whileM, partitionM, concatMapM, concatForM, mconcatMapM, mapMaybeM, findM, firstJustM, fold1M, fold1M_, whenM, unlessM, ifM, notM, (||^), (&&^), orM, andM, anyM, allM,
    -- * Data.Either.Extra
    -- | Extra functions available in @"Data.Either.Extra"@.
    isLeft, isRight, fromLeft, fromRight, fromEither, fromLeft', fromRight', eitherToMaybe, maybeToEither,
    -- * Data.IORef.Extra
    -- | Extra functions available in @"Data.IORef.Extra"@.
    modifyIORef', writeIORef', atomicModifyIORef', atomicWriteIORef, atomicWriteIORef',
    -- * Data.List.Extra
    -- | Extra functions available in @"Data.List.Extra"@.
    lower, upper, trim, trimStart, trimEnd, word1, line1, escapeHTML, escapeJSON, unescapeHTML, unescapeJSON, dropEnd, takeEnd, splitAtEnd, breakEnd, spanEnd, dropWhileEnd, dropWhileEnd', takeWhileEnd, stripSuffix, stripInfix, stripInfixEnd, dropPrefix, dropSuffix, wordsBy, linesBy, breakOn, breakOnEnd, splitOn, split, chunksOf, notNull, list, uncons, unsnoc, cons, snoc, drop1, mconcatMap, groupSort, groupSortOn, groupSortBy, nubOrd, nubOrdBy, nubOrdOn, nubOn, groupOn, sortOn, nubSort, nubSortBy, nubSortOn, maximumOn, minimumOn, disjoint, allSame, anySame, repeatedly, for, firstJust, concatUnzip, concatUnzip3, zipFrom, zipWithFrom, replace, merge, mergeBy,
    -- * Data.Tuple.Extra
    -- | Extra functions available in @"Data.Tuple.Extra"@.
    first, second, (***), (&&&), dupe, both, fst3, snd3, thd3,
    -- * Data.Typeable.Extra
    -- | Extra functions available in @"Data.Typeable.Extra"@.
    typeRep, (:~:)(..), Proxy(..),
    -- * Data.Version.Extra
    -- | Extra functions available in @"Data.Version.Extra"@.
    makeVersion, readVersion,
    -- * Numeric.Extra
    -- | Extra functions available in @"Numeric.Extra"@.
    showDP, intToDouble, intToFloat, floatToDouble, doubleToFloat,
    -- * System.Directory.Extra
    -- | Extra functions available in @"System.Directory.Extra"@.
    withCurrentDirectory, createDirectoryPrivate, listContents, listDirectories, listFiles, listFilesInside, listFilesRecursive,
    -- * System.Environment.Extra
    -- | Extra functions available in @"System.Environment.Extra"@.
    getExecutablePath, lookupEnv,
    -- * System.Info.Extra
    -- | Extra functions available in @"System.Info.Extra"@.
    isWindows, isMac,
    -- * System.IO.Extra
    -- | Extra functions available in @"System.IO.Extra"@.
    captureOutput, withBuffering, readFileEncoding, readFileUTF8, readFileBinary, readFile', readFileEncoding', readFileUTF8', readFileBinary', writeFileEncoding, writeFileUTF8, writeFileBinary, withTempFile, withTempDir, newTempFile, newTempDir, newTempFileWithin, newTempDirWithin, fileEq,
    -- * System.Process.Extra
    -- | Extra functions available in @"System.Process.Extra"@.
    system_, systemOutput, systemOutput_,
    -- * System.Time.Extra
    -- | Extra functions available in @"System.Time.Extra"@.
    Seconds, sleep, timeout, showDuration, offsetTime, offsetTimeIncrease, duration,
    -- * Text.Read.Extra
    -- | Extra functions available in @"Text.Read.Extra"@.
    readEither, readMaybe,
    ) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Either.Extra
import Data.IORef.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Typeable.Extra
import Data.Version.Extra
import Numeric.Extra
import System.Directory.Extra
import System.Environment.Extra
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra
import System.Time.Extra
import Text.Read.Extra
