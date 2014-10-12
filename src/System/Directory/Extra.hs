
module System.Directory.Extra(
    module System.Directory,
    withCurrentDirectory, getDirectoryContentsRecursive
    ) where

import System.Directory
import Control.Monad.Extra
import System.FilePath
import Data.List
import Control.Exception


-- | Remember that the current directory is a global variable, so calling this function
--   multithreaded is almost certain to go wrong. Avoid changing the dir if you can.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive $ sort dirs
    return $ sort files ++ rest
    where
        isBadDir x = "." `isPrefixOf` x -- FIXME, need a version that can also exclude _ dirs
