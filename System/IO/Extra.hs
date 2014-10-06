{-# LANGUAGE ScopedTypeVariables #-}

-- FIXME: Todo
module System.IO.Extra(
    module System.IO, module System.IO.Extra
    ) where

import System.IO
import System.Directory
import Control.Exception as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)



readFile' x = do
    src <- readFile x
    evaluate $ length src
    return src


readFileUtf8' :: FilePath -> IO String
readFileUtf8' x = do
    src <- readFileUtf8 x
    evaluate $ length src
    return src


readFileUtf8 :: FilePath -> IO String
readFileUtf8 x = do
    h <- openFile x ReadMode
    hSetEncoding h utf8
    hGetContents h


readFileLatin1' :: FilePath -> IO String
readFileLatin1' x = do
    src <- readFileLatin1 x
    length src `seq` return src


readFileLatin1 :: FilePath -> IO String
readFileLatin1 x = do
    h <- openFile x ReadMode
    hSetEncoding h latin1
    hGetContents h


writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 x y = withFile x WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h y


writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary x y = withBinaryFile x WriteMode $ \h -> hPutStr h y


withTemporaryFile :: String -> (FilePath -> IO a) -> IO a
withTemporaryFile pat act = do
    tmp <- getTemporaryDirectory
    bracket (openTempFile tmp pat) (removeFile . fst) $
        \(file,h) -> hClose h >> act file

withTemporaryFiles :: String -> Int -> ([FilePath] -> IO a) -> IO a
withTemporaryFiles pat 0 act = act []
withTemporaryFiles pat i act | i > 0 =
    withTemporaryFile pat $ \file ->
        withTemporaryFiles pat (i-1) $ \files ->
            act $ file : files

captureOutput :: IO () -> IO String
captureOutput act = withTemporaryFile "hlint_capture_output.txt" $ \file -> do
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

withBufferMode :: Handle -> BufferMode -> IO a -> IO a
withBufferMode h b act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h LineBuffering
    act

readFileStrict :: FilePath -> IO String
readFileStrict file = withFile file ReadMode $ \h -> do
    src <- hGetContents h
    evaluate $ length src
    return src

readFileUCS2 :: FilePath -> IO String
readFileUCS2 name = openFile name ReadMode >>= \h -> do
    hSetEncoding h utf16
    hGetContents h


removeFile_ x = removeFile x `E.catch` \(_ :: E.SomeException) -> return ()


withDirectory dir cmd = E.bracket
    (do x <- getCurrentDirectory; setCurrentDirectory dir; return x)
    setCurrentDirectory
    (const cmd)
