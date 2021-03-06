-- GENERATED CODE - DO NOT MODIFY
-- See Generate.hs for details of how to generate

-- | This module documents all the functions available in this package.
--
--   Most users should import the specific modules (e.g. @"Data.List.Extra"@), which
--   also reexport their non-@Extra@ modules (e.g. @"Data.List"@).
module Extra {-# DEPRECATED "This module is provided as documentation of all new functions, you should import the more specific modules directly." #-} (
    -- * Control.Concurrent.Extra
    -- | Extra functions available in @"Control.Concurrent.Extra"@.
    withNumCapabilities, once, onceFork, Lock, newLock, withLock, withLockTry, Var, newVar, readVar, writeVar, modifyVar, modifyVar_, withVar, Barrier, newBarrier, signalBarrier, waitBarrier, waitBarrierMaybe,
    -- * Control.Exception.Extra
    -- | Extra functions available in @"Control.Exception.Extra"@.
    Partial, retry, retryBool, errorWithoutStackTrace, showException, stringException, errorIO, ignore, catch_, handle_, try_, catchJust_, handleJust_, tryJust_, catchBool, handleBool, tryBool,
    -- * Control.Monad.Extra
    -- | Extra functions available in @"Control.Monad.Extra"@.
    whenJust, whenJustM, pureIf, whenMaybe, whenMaybeM, unit, maybeM, fromMaybeM, eitherM, loop, loopM, whileM, whileJustM, untilJustM, partitionM, concatMapM, concatForM, mconcatMapM, mapMaybeM, fold1M, fold1M_, whenM, unlessM, ifM, notM, (||^), (&&^),
    -- * Data.Foldable.Extra
    -- | Extra functions available in @"Data.Foldable.Extra"@.
    notNull, sum', product', sumOn', productOn', anyM, allM, orM, andM, findM, firstJust, firstJustM, foldMapM, compareLength, comparingLength, maximumOn, minimumOn,
    -- * Data.Function.Extra
    -- | Extra functions available in @"Data.Function.Extra"@.
    on2, applyN,
    -- * Data.Either.Extra
    -- | Extra functions available in @"Data.Either.Extra"@.
    fromLeft, fromRight, fromEither, fromLeft', fromRight', eitherToMaybe, maybeToEither, mapLeft, mapRight, (|||), (+++),
    -- * Data.IORef.Extra
    -- | Extra functions available in @"Data.IORef.Extra"@.
    writeIORef', atomicWriteIORef', atomicModifyIORef_, atomicModifyIORef'_,
    -- * Data.List.Extra
    -- | Extra functions available in @"Data.List.Extra"@.
    lower, upper, trim, trimStart, trimEnd, word1, line1, escapeHTML, escapeJSON, unescapeHTML, unescapeJSON, dropEnd, takeEnd, splitAtEnd, breakEnd, spanEnd, dropWhileEnd', takeWhileEnd, stripSuffix, stripInfix, stripInfixEnd, dropPrefix, dropSuffix, wordsBy, linesBy, breakOn, breakOnEnd, splitOn, split, chunksOf, headDef, lastDef, list, unsnoc, cons, snoc, drop1, dropEnd1, mconcatMap, enumerate, groupSort, groupSortOn, groupSortBy, nubOrd, nubOrdBy, nubOrdOn, nubOn, groupOn, nubSort, nubSortBy, nubSortOn, disjoint, disjointOrd, disjointOrdBy, allSame, anySame, repeatedly, concatUnzip, concatUnzip3, zipFrom, zipWithFrom, zipWithLongest, replace, merge, mergeBy,
    -- * Data.List.NonEmpty.Extra
    -- | Extra functions available in @"Data.List.NonEmpty.Extra"@.
    (|:), (|>), appendl, appendr, maximum1, minimum1, maximumBy1, minimumBy1, maximumOn1, minimumOn1,
    -- * Data.Monoid.Extra
    -- | Extra functions available in @"Data.Monoid.Extra"@.
    memptyNothing, memptyIf,
    -- * Data.Tuple.Extra
    -- | Extra functions available in @"Data.Tuple.Extra"@.
    first, second, (***), (&&&), dupe, both, firstM, secondM, fst3, snd3, thd3, first3, second3, third3, curry3, uncurry3,
    -- * Data.Version.Extra
    -- | Extra functions available in @"Data.Version.Extra"@.
    readVersion,
    -- * Numeric.Extra
    -- | Extra functions available in @"Numeric.Extra"@.
    showDP, intToDouble, intToFloat, floatToDouble, doubleToFloat,
    -- * System.Directory.Extra
    -- | Extra functions available in @"System.Directory.Extra"@.
    withCurrentDirectory, createDirectoryPrivate, listContents, listDirectories, listFiles, listFilesInside, listFilesRecursive,
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
    ) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Foldable.Extra
import Data.Function.Extra
import Data.Either.Extra
import Data.IORef.Extra
import Data.List.Extra hiding (sum', product', sumOn', productOn', maximumOn, minimumOn, notNull, firstJust)
import Data.List.NonEmpty.Extra hiding (cons, snoc, sortOn, union, unionBy, nubOrd, nubOrdBy, nubOrdOn)
import Data.Monoid.Extra
import Data.Tuple.Extra
import Data.Version.Extra
import Numeric.Extra
import System.Directory.Extra
import System.Info.Extra
import System.IO.Extra
import System.Process.Extra
import System.Time.Extra
