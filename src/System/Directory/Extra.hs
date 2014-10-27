{-# LANGUAGE CPP #-}

module System.Directory.Extra(
    module System.Directory,
    withCurrentDirectory, createDirectoryPrivate,
    listContents, listFiles, listFilesInside, listFilesRecursive
    ) where

import System.Directory
import Control.Monad.Extra
import System.FilePath
import Data.List
import Control.Exception

#ifndef mingw32_HOST_OS
import qualified System.Posix
#endif


-- | Remember that the current directory is a global variable, so calling this function
--   multithreaded is almost certain to go wrong. Avoid changing the dir if you can.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act


-- | List the files in a directory. Each file will be prefixed by the directory,
--   and will skip @.@ and @..@ functions. A cleaned up version of 'getDirectoryContents'.
listContents :: FilePath -> IO [FilePath]
listContents dir = do
    xs <- getDirectoryContents dir
    return [dir </> x | x <- xs, not $ all (== '.') x]

-- | Like 'listContents', but only returns files, not directories.
listFiles :: FilePath -> IO [FilePath]
listFiles dir = filterM doesFileExist =<< listContents dir


-- | Like 'listFiles' but ago goes recursively.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = listFilesInside (const $ return True)


-- | Like 'listFilesRecursive', but chose where to recurse into.
--   Typically directories starting with @.@ would be ignored.
listFilesInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listFilesInside test dir = do
    (dirs,files) <- partitionM doesDirectoryExist =<< listContents dir
    dirs <- filterM test dirs
    rest <- concatMapM (listFilesInside test) $ sort dirs
    return $ sort files ++ dirs


-- | Create a directory with permissions so that only the current user can view it.
--   On Windows this function is equivalent to 'createDirectory'.
createDirectoryPrivate :: String -> IO ()
#ifdef mingw32_HOST_OS
createDirectoryPrivate s = createDirectory s
#else
createDirectoryPrivate s = System.Posix.createDirectory s 0o700
#endif
