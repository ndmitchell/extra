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


-- | List the files and directories directly within a directory.
--   Each result will be prefixed by the query directory, and the special directories @.@ and @..@ will be ignored.
--   Intended as a cleaned up version of 'getDirectoryContents'.
--
-- > withTempDir $ \dir -> do writeFile (dir </> "test.txt") ""; (== [dir </> "test.txt"]) <$> listContents dir
-- > let touch = mapM_ $ \x -> createDirectoryIfMissing True (takeDirectory x) >> writeFile x ""
-- > let listTest op as bs = withTempDir $ \dir -> do touch $ map (dir </>) as; res <- op dir; return $ map (drop (length dir + 1)) res == bs
-- > listTest listContents ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","foo","zoo"]
listContents :: FilePath -> IO [FilePath]
listContents dir = do
    xs <- getDirectoryContents dir
    return $ sort [dir </> x | x <- xs, not $ all (== '.') x]

-- | Like 'listContents', but only returns the files in a directory, not other directories.
--   Each file will be prefixed by the query directory.
--
-- > listTest listFiles ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","zoo"]
listFiles :: FilePath -> IO [FilePath]
listFiles dir = filterM doesFileExist =<< listContents dir


-- | Like 'listFiles', but ago goes recursively through all subdirectories.
--
-- > listTest listFilesRecursive ["bar.txt","zoo","foo" </> "baz.txt"] ["bar.txt","zoo","foo" </> "baz.txt"]
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive = listFilesInside (const $ return True)


-- | Like 'listFilesRecursive', but with a predicate to decide where to recurse into.
--   Typically directories starting with @.@ would be ignored. The initial argument directory
--   will have the test applied to it.
--
-- > listTest (listFilesInside $ return . not . isPrefixOf "." . takeFileName) ["bar.txt","foo" </> "baz.txt",".foo" </> "baz2.txt", "zoo"] ["bar.txt","zoo","foo" </> "baz.txt"]
-- > listTest (listFilesInside $ const $ return False) ["bar.txt"] []
listFilesInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listFilesInside test dir = ifM (notM $ test dir) (return []) $ do
    (dirs,files) <- partitionM doesDirectoryExist =<< listContents dir
    rest <- concatMapM (listFilesInside test) dirs
    return $ files ++ rest


-- | Create a directory with permissions so that only the current user can view it.
--   On Windows this function is equivalent to 'createDirectory'.
createDirectoryPrivate :: String -> IO ()
#ifdef mingw32_HOST_OS
createDirectoryPrivate s = createDirectory s
#else
createDirectoryPrivate s = System.Posix.createDirectory s 0o700
#endif
