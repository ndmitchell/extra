module Data.Function.Extra (
    module Data.Function,
    on2,
) where

import Data.Function

-- | Like 'on', but avoids recalculating values. Especially useful in folds.
on2 :: (a -> a -> c) -> (b -> a) -> b -> b -> c
(•) `on2` f = \x -> let fx = f x in (fx •) . f
