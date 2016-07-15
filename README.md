# Extra [![Hackage version](https://img.shields.io/hackage/v/extra.svg?label=Hackage)](https://hackage.haskell.org/package/extra) [![Stackage version](https://www.stackage.org/package/extra/badge/lts?label=Stackage)](https://www.stackage.org/package/extra) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/extra.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/extra) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/extra.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/extra)

A library of extra functions for the standard Haskell libraries. Most functions are simple additions, filling out missing functionality. A few functions are available in later versions of GHC, but this package makes them available back to GHC 7.2. A few examples:

* `Control.Monad.Extra.concatMapM` provides a monadic version of `concatMap`, in the same way that `mapM` is a monadic version of `map`.
* `Data.Tuple.Extra.fst3` provides a function to get the first element of a triple.
* `Control.Exception.Extra.retry` provides a function that retries an `IO` action a number of times.
* `System.Environment.Extra.lookupEnv` is a function available in GHC 7.6 and above. On GHC 7.6 and above this package reexports the version from `System.Environment` while on GHC 7.4 and below it defines an equivalent version.

The module `Extra` documents all functions provided by this library. Modules such as `Data.List.Extra` provide extra functions over `Data.List` and also reexport `Data.List`. Users are recommended to replace `Data.List` imports with `Data.List.Extra` if they need the extra functionality.


## Which functions?

When producing a library of extra functions I have been guided by a few principles. I encourage others with small useful utility functions contribute them here, perhaps as a temporary stop before proposing they join the standard libraries.

* I have been using most of these functions in my packages - they have proved useful enough to be worth copying/pasting into each project.
* The functions follow the spirit of the original Prelude/base libraries. I am happy to provide partial functions (e.g. `fromRight`), and functions which are specialisations of more generic functions (`whenJust`).
* Most of the functions have trivial implementations. If a beginner couldn't write the function, it probably doesn't belong here.
* I have defined only a few new data types or type aliases. It's a package for defining new utilities on existing types, not new types or concepts.


## Base versions

The following GHC versions correspond to the following base library versions:

* base 4.9 == GHC 8.0
* base 4.8 == GHC 7.10
* base 4.7 == GHC 7.8
* base 4.6 == GHC 7.6
* base 4.5 == GHC 7.4
* base 4.4 == GHC 7.2

A more complete list can be found [here](https://wiki.haskell.org/Base_package#Versions).
