
module Test(main) where

import TestGen
import TestUtil

main :: IO ()
main = runTests $ tests
