
module System.Directory.Extra(
    module System.Directory,
    withCurrentDirectory, getDirectoryContentsRecursive, getDirectoryContentsRecursiveWith
    ) where

import System.Directory
import Control.Monad.Extra
import System.FilePath
import Data.List
import Control.Exception


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
        isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x -- FIXME

getDirectoryContentsRecursiveWith :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getDirectoryContentsRecursiveWith = undefined

