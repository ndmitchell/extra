{-# LANGUAGE ScopedTypeVariables #-}

-- | More advanced temporary file manipulation functions can be found in the @exceptions@ package.
module System.IO.Extra(
    module System.IO,
    readFileEncoding, readFileUTF8, readFileBinary,
    readFile', readFileEncoding', readFileUTF8', readFileBinary',
    writeFileEncoding, writeFileUTF8, writeFileBinary,
    withTempFile, withTempDir, newTempFile, newTempDir,
    captureOutput,
    withBuffering,
    ) where

import System.IO
import Control.Exception.Extra as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import System.Directory.Extra
import System.IO.Error
import System.FilePath
import Data.Char
import Data.Time.Clock


-- File reading

-- | Like 'readFile', but setting an encoding.
readFileEncoding :: TextEncoding -> FilePath -> IO String
readFileEncoding enc file = do
    h <- openFile file ReadMode
    hSetEncoding h enc
    hGetContents h

-- | Like 'readFile', but with the encoding 'utf8'.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 = readFileEncoding utf8

-- | Like 'readFile', but for binary files.
readFileBinary :: FilePath -> IO String
readFileBinary file = do
    h <- openBinaryFile file ReadMode
    hGetContents h

-- Strict file reading

-- | A strict version of 'readFile'. When the string is produced, the entire
--   file will have been read into memory and the file handle will have been closed.
--   Closing the file handle does not rely on the garbage collector.
readFile' :: FilePath -> IO String
readFile' file = withFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

-- | A strict version of 'readFileEncoding', see 'readFile'' for details.
readFileEncoding' :: TextEncoding -> FilePath -> IO String
readFileEncoding' e file = withFile file ReadMode $ \h -> do
    hSetEncoding h e
    s <- hGetContents h
    evaluate $ length s
    return s

-- | A strict version of 'readFileUTF8', see 'readFile'' for details.
readFileUTF8' :: FilePath -> IO String
readFileUTF8' = readFileEncoding' utf8

-- | A strict version of 'readFileBinary', see 'readFile'' for details.
readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

-- File writing

-- | Write a file with a particular encoding.
writeFileEncoding :: TextEncoding -> FilePath -> String -> IO ()
writeFileEncoding enc file x = withFile x WriteMode $ \h -> do
    hSetEncoding h enc
    hPutStr h x

-- | Write a file with the 'utf8' encoding.
writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 = writeFileEncoding utf8

-- | Write a binary file.
writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

-- Console

-- | Capture the 'stdout' and 'stderr' of a computation.
--
-- > captureOutput (print 1) == return ("1\n",())
captureOutput :: IO a -> IO (String, a)
captureOutput act = withTempFile $ \file -> do
    withFile file ReadWriteMode $ \h -> do
        res <- clone stdout h $ clone stderr h $ do
            hClose h
            act
        out <- readFile' file
        return (out, res)
    where
        clone out h act = do
            buf <- hGetBuffering out
            out2 <- hDuplicate out
            hDuplicateTo h out
            act `finally` do
                hDuplicateTo out2 out
                hSetBuffering out buf


-- | Execute an action with a custom 'BufferMode', a warpper around
--   'hSetBuffering'.
withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h m act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h m
    act

-- Temporary file

-- | Provide a function to create a temporary file, and a way to delete a
--   temporary file. Most users should use 'withTempFile' which
--   combines these operations.
newTempFile :: (IO FilePath, FilePath -> IO ())
newTempFile = (create, ignore . removeFile)
    where
        create = do
            tmpdir <- getTemporaryDirectory
            (file, h) <- openTempFile tmpdir "extra"
            hClose h
            return file


-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes. The 'FilePath' will not have any file extension.
--   If you require a file with a specific name, use 'withTempDir'.
withTempFile :: (FilePath -> IO a) -> IO a
withTempFile = uncurry bracket newTempFile


-- | Provide a function to create a temporary directory, and a way to delete a
--   temporary directory. Most users should use 'withTempDir' which
--   combines these operations.
newTempDir :: (IO FilePath, FilePath -> IO ())
newTempDir = (create, ignore . removeDirectoryRecursive)
    where
        create = do
            tmpdir <- getTemporaryDirectory
            -- get the number of seconds during today (including floating point), and grab some interesting digits
            rand :: Integer <- fmap (read . take 20 . filter isDigit . show . utctDayTime) getCurrentTime
            find tmpdir rand

        find tmpdir x = do
            let dir = tmpdir </> "extra" ++ show x
            catchBool isAlreadyExistsError
                (createDirectoryPrivate dir >> return dir) $
                \e -> find tmpdir (x+1)


-- | Create a temporary directory inside the system temporary directory.
--   The directory will be deleted after the action completes.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = uncurry bracket newTempDir
