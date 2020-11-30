-- | This module re-exports "Data.Bool", adding a common function 'justIf'
--   to wrap a value in a `Just` if the condition evaluates to `True`, and `Nothing` otherwise.
module Data.Bool.Extra (
    module Data.Bool,
    justIf,
) where

import Data.Bool

-- | Return either the value wrapped in a `Just`, or `Nothing` if the condition is false.
--
-- > justIf True  5 == Just 5
-- > justIf False 5 == Nothing
justIf :: Bool -> a -> Maybe a
justIf b a = if b then Just a else Nothing