module Data.Bool.Extra (
    module Data.Bool,
    justIf,
    (<?),
    (?>),
) where

import Data.Bool

infix 1 <?, ?>, `justIf`

-- | Return either the value wrapped in a `Just`, or `Nothing` if the condition is false.
--
-- `(<?)` has a lower fixity than both `(&&)` and `(||)`, to avoid overbracketing.
--
-- > justIf 5 True == Just 5
-- > justIf 5 False == Nothing
justIf :: a -> Bool -> Maybe a
justIf a b = if b then Just a else Nothing

-- | Infix variant of `justIf`.
(<?) :: a -> Bool -> Maybe a
(<?) = justIf

-- | Flipped variant of `(<?)`, with the same fixity.
(?>) :: Bool -> a -> Maybe a
(?>) = flip (<?)
