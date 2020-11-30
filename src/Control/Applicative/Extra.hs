-- | This module extends "Control.Applicative" with extra functions.
module Control.Applicative.Extra (
    module Control.Applicative,
    pureIf,
) where

import Control.Applicative

-- | A generalized version of 'Data.Bool.Extra.justIf'.
pureIf :: (Alternative m) => Bool -> a -> m a
pureIf b a = if b then pure a else empty