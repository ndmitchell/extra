module Test (main) where

import TestCustom
import TestGen
import TestUtil

-- Check that we managed to export everything
_unused1 :: Applicative m => p -> Maybe a -> (a -> m ()) -> m ()
_unused1 _ = whenJust

_unused2 :: Monad m => p -> m Bool -> m Bool -> m Bool
_unused2 _ = (&&^)

_unused3 :: p -> String -> IO ()
_unused3 _ = system_

_unused4 :: p -> String -> (String, String)
_unused4 _ = word1

_unused5 :: p -> FilePath -> IO String
_unused5 _ = readFile'

_unused6 :: Seconds -> Seconds
_unused6 x = x :: Seconds

main :: IO ()
main = runTests $ do
    tests
    testCustom
