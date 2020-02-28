{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_directory
#if __GLASGOW_HASKELL__ >= 711
#define MIN_VERSION_directory(a,b,c) 1
#else
#define MIN_VERSION_directory(a,b,c) 0
#endif
#endif


-- | Extra directory functions. Most of these functions provide cleaned up and generalised versions
--   of 'getDirectoryContents', see 'listContents' for the differences.
module System.Directory.Extra(
    module System.Directory,
#if !MIN_VERSION_directory(1,2,3)
    withCurrentDirectory,
#endif
    createDirectoryPrivate,
    listContents, listDirectories, listFiles, listFilesInside, listFilesRecursive
    ) where

import System.Directory
import Control.Monad.Extra
import System.FilePath
import Data.List
#if !MIN_VERSION_directory(1,2,3)
import Control.Exception
#endif

#ifndef mingw32_HOST_OS
import qualified System.Posix
#endif


#if !MIN_VERSION_directory(1,2,3)
-- | Set the current directory, perform an operation, then change back.
--   Remember that the current directory is a global variable, so calling this function
--   multithreaded is almost certain to go wrong. Avoid changing the current directory if you can.
--
-- > withTempDir $ \dir -> do writeFile (dir </> "foo.txt") ""; withCurrentDirectory dir $ doesFileExist "foo.txt"
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act
#endif


-- | List the files and directories directly within a directory.
--   Each result will be prefixed by the query directory, and the special directories @.@ and @..@ will be ignored.
--   Intended as a cleaned up version of 'getDirectoryContents'.
--
-- > withTempDir $ \dir -> do writeFile (dir </> "test.txt") ""; (== [dir </> "test.txt"]) <$> listContents dir
-- > let touch = mapM_ $ \x -> createDirectoryIfMissing True (takeDirectory x) >> writeFile x ""
-- > let listTest op as bs = withTempDir $ \dir -> do touch $ map (dir </>) as; res <- op dir; pure $ map (drop (length dir + 1)) res == bs
-- > listTest listContents ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","foo","zoo"]
listContents :: FilePath -> IO [FilePath]
listContents dir = do
    xs <- getDirectoryContents dir
    pure $ sort [dir </> x | x <- xs, not $ all (== '.') x]


-- | Like 'listContents', but only returns the directories in a directory, not the files.
--   Each directory will be prefixed by the query directory.
--
-- > listTest listDirectories ["bar.txt","foo/baz.txt","zoo"] ["foo"]
listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = filterM doesDirectoryExist =<< listContents dir


-- | Like 'listContents', but only returns the files in a directory, not other directories.
--   Each file will be prefixed by the query directory.
--
-- > listTest listFiles ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","zoo"]
listFiles :: FilePath -> IO [FilePath]
listFiles dir = filterM doesFileExist =<< listContents dir


-- | Like 'listFiles', but goes recursively through all subdirectories.
--   This function will follow symlinks, and if they form a loop, this function will not terminate.
--
-- > listTest listFilesRecursive ["bar.txt","zoo","foo" </> "baz.txt"] ["bar.txt","zoo","foo" </> "baz.txt"]
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = listFilesInside (const $ pure True)


-- | Like 'listFilesRecursive', but with a predicate to decide where to recurse into.
--   Typically directories starting with @.@ would be ignored. The initial argument directory
--   will have the test applied to it.
--
-- > listTest (listFilesInside $ pure . not . isPrefixOf "." . takeFileName)
-- >     ["bar.txt","foo" </> "baz.txt",".foo" </> "baz2.txt", "zoo"] ["bar.txt","zoo","foo" </> "baz.txt"]
-- > listTest (listFilesInside $ const $ pure False) ["bar.txt"] []
listFilesInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listFilesInside test dir = ifM (notM $ test $ dropTrailingPathSeparator dir) (pure []) $ do
    (dirs,files) <- partitionM doesDirectoryExist =<< listContents dir
    rest <- concatMapM (listFilesInside test) dirs
    pure $ files ++ rest


-- | Create a directory with permissions so that only the current user can view it.
--   On Windows this function is equivalent to 'createDirectory'.
createDirectoryPrivate :: String -> IO ()
#ifdef mingw32_HOST_OS
createDirectoryPrivate s = createDirectory s
#else
createDirectoryPrivate s = System.Posix.createDirectory s 0o700
#endif
