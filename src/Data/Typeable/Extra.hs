
-- | This module extends "Data.Typeable" with extra functions available in later GHC versions.
--   The package also exports the existing "Data.Typeable" functions.
--
--   Currently this module has no functionality beyond "Data.Typeable".
module Data.Typeable.Extra {-# DEPRECATED "Use Data.Typeable directly" #-} (
    module Data.Typeable
    ) where

import Data.Typeable
