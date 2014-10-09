module Extra(
    -- * Control.Concurrent.Extra
    withNumCapabilities, setNumCapabilities,
    -- * Control.Exception.Extra
    retry, showException, ignore, catch_, handle_, try_, catchJust_, handleJust_, tryJust_, catchBool, handleBool, tryBool,
    -- * Control.Monad.Extra
    whenJust, unit, partitionM, concatMapM, loopM, whileM, ifM, notM, (||^), (&&^), orM, andM, anyM, allM, findM, firstJustM,
    -- * Data.Either.Extra
    isLeft, isRight, fromLeft, fromRight, fromEither,
    -- * Data.IORef.Extra
    modifyIORef', writeIORef', atomicModifyIORef', atomicWriteIORef, atomicWriteIORef',
    -- * Data.List.Extra
    lower, upper, trim, trimStart, trimEnd, dropAround, word1, drop1, list, uncons, unsnoc, cons, snoc, groupSort, groupSortOn, nubOn, groupOn, sortOn, chop, for, rep, reps, disjoint, distinct, dropEnd, takeEnd, breakEnd, spanEnd, dropWhileEnd, takeWhileEnd, stripSuffix, concatUnzip, merge, mergeBy, replace, wordsBy, linesBy, firstJust, breakOn, breakOnEnd, splitOn, split, chunksOf,
    -- * Data.Tuple.Extra
    dupe, fst3, snd3, thd3, concat2, concat3,
    -- * Numeric.Extra
    showDP, intToDouble, intToFloat, floatToDouble, doubleToFloat,
    -- * System.Directory.Extra
    withCurrentDirectory, getDirectoryContentsRecursive, createDirectoryPrivate,
    -- * System.Environment.Extra
    getExecutablePath, lookupEnv,
    -- * System.Info.Extra
    isWindows, getProcessorCount,
    -- * System.IO.Extra
    readFileEncoding, readFileUTF8, readFileBinary, readFile', readFileEncoding', readFileUTF8', readFileBinary', writeFileEncoding, writeFileUTF8, writeFileBinary, withTempFile, withTempDir, newTempFile, newTempDir, captureOutput, withBuffering,
    -- * System.Time.Extra
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
