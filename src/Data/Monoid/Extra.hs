-- | This module re-exports functions and data types from Data.Monoid,
--   as well as a few extra helper functions.
module Data.Monoid.Extra (
    module Data.Monoid,
    memptyNothing,
    memptyIf,
) where

import Control.Monad.Extra (pureIf)
import Data.Monoid

-- | Discard a `mempty` value.
--
-- > memptyNothing "" == Nothing
-- > memptyNothing "abc" == Just "abc"
-- > memptyNothing (Sum 0) == Nothing
-- > memptyNothing (Sum 1) == Just (Sum 1)
memptyNothing :: (Eq a, Monoid a) => a -> Maybe a
memptyNothing a = pureIf (a /= mempty) a

-- | Return `mempty` instead of the given value if the test succeeds.
--
-- > memptyIf True "abc" == ""
-- > memptyIf False "abc" == "abc"
-- > memptyIf True (Sum 1) == Sum 0
-- > memptyIf False (Sum 1) == Sum 1
memptyIf :: (Monoid a) => Bool -> a -> a
memptyIf b a = if b then mempty else a
