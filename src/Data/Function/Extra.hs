-- | Extra functions for "Data.Function".
--   These functions provide conversions between function types.
module Data.Function.Extra(
    stateMod, stateGet,
    stArrowState, stArrowMod, stArrowGet,
    module Data.Function
    ) where

import Data.Function

-- | Convert a modification function into a state transition function.
stateMod :: (s -> s) -> (s -> ((), s))
stateMod f s = ((), f s)
{-# INLINE stateMod #-}

-- | Convert a getter function into a state transition function.
stateGet :: (s -> o) -> (s -> (o, s))
stateGet f s = (f s, s)
{-# INLINE stateGet #-}

-- | Convert a state transition function into a state transition arrow.
stArrowState :: (s -> os) -> ((i, s) -> os)
stArrowState f = f . snd
{-# INLINE stArrowState #-}

-- | Convert a modification arrow into a state transition arrow.
stArrowMod :: (i -> s -> s) -> ((i, s) -> ((), s))
stArrowMod f = stateMod id . uncurry f
{-# INLINE stArrowMod #-}

-- | Convert a getter function into a state transition arrow.
stArrowGet :: (s -> a) -> ((i, s) -> (a, s))
stArrowGet f = stateGet f . snd
{-# INLINE stArrowGet #-}
