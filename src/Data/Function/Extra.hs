module Data.Function.Extra (
    module Data.Function,
    on2,
    applyN,
) where

import Data.Function
import Data.Monoid (Endo (..))
import Data.Semigroup (stimesMonoid)

-- | Like 'on', but avoids recalculating values. Especially useful in folds.
on2 :: (a -> a -> c) -> (b -> a) -> b -> b -> c
(•) `on2` f = \x -> let fx = f x in (fx •) . f

applyN :: Integral n => n -> (a -> a) -> (a -> a)
applyN n = appEndo . stimesMonoid n . Endo
