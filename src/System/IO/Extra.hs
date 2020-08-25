{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | More IO functions. The functions include ones for reading files with specific encodings,
--   strictly reading files, and writing files with encodings. There are also some simple
--   temporary file functions, more advanced alternatives can be found in
--   the <https://hackage.haskell.org/package/exceptions exceptions> package.
module System.IO.Extra(
    module System.IO,
    captureOutput,
    withBuffering,
    -- * Read encoding
    readFileEncoding, readFileUTF8, readFileBinary,
    -- * Strict reading
    readFile', readFileEncoding', readFileUTF8', readFileBinary',
    -- * Write with encoding
    writeFileEncoding, writeFileUTF8, writeFileBinary,
    -- * Temporary files
    withTempFile, withTempDir, newTempFile, newTempDir,
    newTempFileWithin, newTempDirWithin,
    -- * File comparison
    fileEq,
    ) where

import System.IO
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Exception.Extra as E
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
import System.Directory.Extra
import System.IO.Error
import System.IO.Unsafe
import System.FilePath
import Data.Char
import Data.Time.Clock
import Data.Tuple.Extra
import Data.IORef
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Data.Functor
import Prelude

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

#if __GLASGOW_HASKELL__ < 811
-- readFile' and hGetContents' were added in GHC 9.0

-- | A strict version of 'hGetContents'.
hGetContents' :: Handle -> IO String
hGetContents' h = do
    s <- hGetContents h
    void $ evaluate $ length s
    pure s

-- | A strict version of 'readFile'. When the string is produced, the entire
--   file will have been read into memory and the file handle will have been closed.
--   Closing the file handle does not rely on the garbage collector.
--
-- > \(filter isHexDigit -> s) -> fmap (== s) $ withTempFile $ \file -> do writeFile file s; readFile' file
readFile' :: FilePath -> IO String
readFile' file = withFile file ReadMode hGetContents'

#endif

-- | A strict version of 'readFileEncoding', see 'readFile'' for details.
readFileEncoding' :: TextEncoding -> FilePath -> IO String
readFileEncoding' e file = withFile file ReadMode $ \h -> hSetEncoding h e >> hGetContents' h

-- | A strict version of 'readFileUTF8', see 'readFile'' for details.
readFileUTF8' :: FilePath -> IO String
readFileUTF8' = readFileEncoding' utf8

-- | A strict version of 'readFileBinary', see 'readFile'' for details.
readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode hGetContents'

-- File writing

-- | Write a file with a particular encoding.
writeFileEncoding :: TextEncoding -> FilePath -> String -> IO ()
writeFileEncoding enc file x = withFile file WriteMode $ \h -> do
    hSetEncoding h enc
    hPutStr h x

-- | Write a file with the 'utf8' encoding.
--
-- > \s -> withTempFile $ \file -> do writeFileUTF8 file s; fmap (== s) $ readFileUTF8' file
writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 = writeFileEncoding utf8

-- | Write a binary file.
--
-- > \(ASCIIString s) -> withTempFile $ \file -> do writeFileBinary file s; fmap (== s) $ readFileBinary' file
writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

-- Console

-- | Capture the 'stdout' and 'stderr' of a computation.
--
-- > captureOutput (print 1) == pure ("1\n",())
captureOutput :: IO a -> IO (String, a)
captureOutput act = withTempFile $ \file ->
    withFile file ReadWriteMode $ \h -> do
        res <- clone stdout h $ clone stderr h $ do
            hClose h
            act
        out <- readFile' file
        pure (out, res)
    where
        clone out h act = do
            buf <- hGetBuffering out
            out2 <- hDuplicate out
            hDuplicateTo h out
            act `finally` do
                hDuplicateTo out2 out
                hClose out2
                hSetBuffering out buf


-- | Execute an action with a custom 'BufferMode', a wrapper around
--   'hSetBuffering'.
withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h m act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h m
    act


---------------------------------------------------------------------
-- TEMPORARY FILE
-- We don't use GHC's temp file code, because its buggy, see:
-- https://ghc.haskell.org/trac/ghc/ticket/10731

{-# NOINLINE tempRef #-}
tempRef :: IORef Int
tempRef = unsafePerformIO $ do
    rand :: Integer <- fmap (read . reverse . filter isDigit . show . utctDayTime) getCurrentTime
    newIORef $ fromIntegral rand

tempUnique :: IO Int
tempUnique = atomicModifyIORef tempRef $ succ &&& succ


-- | Provide a function to create a temporary file, and a way to delete a
--   temporary file. Most users should use 'withTempFile' which
--   combines these operations.
newTempFile :: IO (FilePath, IO ())
newTempFile = newTempFileWithin =<< getTemporaryDirectory

-- | Like 'newTempFile' but using a custom temporary directory.
newTempFileWithin :: FilePath -> IO (FilePath, IO ())
newTempFileWithin tmpdir = do
        file <- create
        del <- once $ ignore $ removeFile file
        pure (file, del)
    where
        create = do
            val <- tempUnique
            (file, h) <- retryBool (\(_ :: IOError) -> True) 5 $ openTempFile tmpdir $ "extra-file-" ++ show val ++ "-"
            hClose h
            pure file


-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes (provided the file is not still open).
--   The 'FilePath' will not have any file extension, will exist, and will be zero bytes long.
--   If you require a file with a specific name, use 'withTempDir'.
--
-- > withTempFile doesFileExist == pure True
-- > (doesFileExist =<< withTempFile pure) == pure False
-- > withTempFile readFile' == pure ""
withTempFile :: (FilePath -> IO a) -> IO a
withTempFile act = do
    (file, del) <- newTempFile
    act file `finally` del


-- | Provide a function to create a temporary directory, and a way to delete a
--   temporary directory. Most users should use 'withTempDir' which
--   combines these operations.
newTempDir :: IO (FilePath, IO ())
newTempDir = newTempDirWithin =<< getTemporaryDirectory

-- | Like 'newTempDir' but using a custom temporary directory.
newTempDirWithin :: FilePath -> IO (FilePath, IO ())
newTempDirWithin tmpdir = do
        dir <- retryBool (\(_ :: IOError) -> True) 5 $ create tmpdir
        del <- once $ ignore $ removeDirectoryRecursive dir
        pure (dir, del)
    where
        create tmpdir = do
            v <- tempUnique
            let dir = tmpdir </> "extra-dir-" ++ show v
            catchBool isAlreadyExistsError
                (createDirectoryPrivate dir >> pure dir) $
                \_ -> create tmpdir


-- | Create a temporary directory inside the system temporary directory.
--   The directory will be deleted after the action completes.
--
-- > withTempDir doesDirectoryExist == pure True
-- > (doesDirectoryExist =<< withTempDir pure) == pure False
-- > withTempDir listFiles == pure []
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
    (dir,del) <- newTempDir
    act dir `finally` del

-- | Returns 'True' when both files have the same size.
sameSize :: Handle -> Handle -> IO Bool
sameSize h1 h2 = liftM2 (==) (hFileSize h1) (hFileSize h2)

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr CUChar -> Ptr CUChar -> CSize -> IO CInt

-- | Returns 'True' when the contents of both files is the same.
sameContent :: Handle -> Handle -> IO Bool
sameContent h1 h2 = sameSize h1 h2 &&^ withb (\b1 -> withb $ \b2 -> eq b1 b2)
    where eq b1 b2 = do
            r1 <- hGetBuf h1 b1 bufsz
            r2 <- hGetBuf h2 b2 bufsz
            if r1 == 0
                then pure $ r2 == 0
                else pure (r1 == r2) &&^ bufeq b1 b2 r1 &&^ eq b1 b2
          bufeq b1 b2 s = (==0) <$> memcmp b1 b2 (fromIntegral s)
          withb = allocaBytesAligned bufsz 4096
          bufsz = 64*1024

-- | Returns 'True' if both files have the same content.
--   Raises an error if either file is missing.
--
-- > fileEq "does_not_exist1" "does_not_exist2" == undefined
-- > fileEq "does_not_exist" "does_not_exist" == undefined
-- > withTempFile $ \f1 -> fileEq "does_not_exist" f1 == undefined
-- > withTempFile $ \f1 -> withTempFile $ \f2 -> fileEq f1 f2
-- > withTempFile $ \f1 -> withTempFile $ \f2 -> writeFile f1 "a" >> writeFile f2 "a" >> fileEq f1 f2
-- > withTempFile $ \f1 -> withTempFile $ \f2 -> writeFile f1 "a" >> writeFile f2 "b" >> notM (fileEq f1 f2)
fileEq :: FilePath -> FilePath -> IO Bool
fileEq p1 p2 = withH p1 $ \h1 -> withH p2 $ \h2 -> sameContent h1 h2
    where withH p = withBinaryFile p ReadMode
