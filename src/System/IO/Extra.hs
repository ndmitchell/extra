{-# LANGUAGE ScopedTypeVariables, CPP #-}

-- | More advanced temporary file manipulation functions can be found in the @exceptions@ package.
module System.IO.Extra(
    module System.IO,
    readFileEncoding, readFileUTF8, readFileBinary,
    readFile', readFileEncoding', readFileUTF8', readFileBinary',
    writeFileEncoding, writeFileUTF8, writeFileBinary,
    withTempFile, withTempDir,
    captureOutput,
    withBuffering,
    ) where

import System.IO
import Control.Exception.Extra as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import System.Directory
import System.IO.Error
import System.FilePath
import Data.Char
import Data.Time.Clock

#ifndef mingw32_HOST_OS
import qualified System.Posix
#endif


-- File reading

readFileEncoding :: TextEncoding -> FilePath -> IO String
readFileEncoding enc file = do
    h <- openFile file ReadMode
    hSetEncoding h enc
    hGetContents h

readFileUTF8 :: FilePath -> IO String
readFileUTF8 = readFileEncoding utf8

readFileBinary :: FilePath -> IO String
readFileBinary file = do
    h <- openBinaryFile file ReadMode
    hGetContents h

-- Strict file reading

readFile' :: FilePath -> IO String
readFile' file = withFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

readFileEncoding' :: TextEncoding -> FilePath -> IO String
readFileEncoding' e file = withFile file ReadMode $ \h -> do
    hSetEncoding h e
    s <- hGetContents h
    evaluate $ length s
    return s

readFileUTF8' :: FilePath -> IO String
readFileUTF8' = readFileEncoding' utf8

readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

-- File writing

writeFileEncoding :: TextEncoding -> FilePath -> String -> IO ()
writeFileEncoding enc file x = withFile x WriteMode $ \h -> do
    hSetEncoding h enc
    hPutStr h x

writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 = writeFileEncoding utf8

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

-- Other

captureOutput :: IO () -> IO String
captureOutput act = withTempFile $ \file -> do
    h <- openFile file ReadWriteMode
    bout <- hGetBuffering stdout
    berr <- hGetBuffering stderr
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    hSetBuffering stdout bout
    hSetBuffering stderr berr
    readFile' file


withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h m act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h m
    act


withTempFile :: (FilePath -> IO a) -> IO a
withTempFile act = do
    tmpdir <- getTemporaryDirectory
    bracket
        (openTempFile tmpdir "extra")
        (\(file, h) -> ignore $ removeFile file)
        (\(file, h) -> hClose h >> act file)


withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
    tmpdir <- getTemporaryDirectory
    bracket
        (createTempDirectory tmpdir "extra")
        (ignore . removeDirectoryRecursive)
        act


createTempDirectory :: FilePath -> String -> IO FilePath
createTempDirectory dir prefix = do
    -- get the number of seconds during today (including floating point), and grab some interesting digits
    rand :: Integer <- fmap (read . take 20 . filter isDigit . show . utctDayTime) getCurrentTime
    findTempName rand
    where
        findTempName x = do
            let dirpath = dir </> prefix ++ show x
            catchBool isAlreadyExistsError
                (mkPrivateDir dirpath >> return dirpath) $
                \e -> findTempName (x+1)

mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif
