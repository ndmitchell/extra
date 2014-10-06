{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Extra(
    module System.IO,
    readFileEncoding, readFileUTF8, readFileBinary,
    readFile', readFileEncoding', readFileUTF8', readFileBinary',
    writeFileEncoding, writeFileUTF8, writeFileBinary,
    withTemporaryFile,
    captureOutput,
    withBuffering,
    ) where

import System.IO
import System.Directory
import Control.Exception as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)


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

withTemporaryFile :: String -> (FilePath -> IO a) -> IO a
withTemporaryFile pat act = do
    tmp <- getTemporaryDirectory
    bracket (openTempFile tmp pat) (removeFile . fst) $
        \(file,h) -> hClose h >> act file


captureOutput :: IO () -> IO String
captureOutput act = withTemporaryFile "extra-capture.txt" $ \file -> do
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
