{-# LANGUAGE TupleSections #-}

-- | Extra functions for working with monoids.
module Data.Monoid.Extra(
    module Data.Monoid,
    -- * Extra operations
    mwhen
    ) where

import Data.Monoid

-- | Like 'Control.Monad.when', but operating on a 'Monoid'. If the first argument
--   is 'True' returns the second, otherwise returns 'mempty'.
--
-- > mwhen True  "test" == "test"
-- > mwhen False "test" == ""
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty
