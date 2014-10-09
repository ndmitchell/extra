
module Test(main) where

import TestGen
import TestUtil

-- Check that we managed to export everything
import Extra(Seconds, whenJust, (&&^), system_)
_unused1 x = whenJust x
_unused2 x = (&&^) x
_unused3 x = system_ x
_unused4 x = x :: Seconds


main :: IO ()
main = runTests $ tests
