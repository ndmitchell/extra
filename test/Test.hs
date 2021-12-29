
module Test(main) where

import TestGen
import TestUtil
import TestCustom

-- Check that we managed to export everything
_unused1 x = whenJust
_unused2 x = (&&^)
_unused3 x = system_
_unused4 x = word1
_unused5 x = readFile'
_unused6 x = x :: Seconds


main :: IO ()
main = runTests $ do
    testSetup
    tests
    testCustom
