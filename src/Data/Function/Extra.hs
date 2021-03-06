module Data.Function.Extra (
    module Data.Function,
    on2,
    applyN,
    applyWhen,
    (.:),
) where

import Data.Function
import Data.Monoid (Endo (..))
import Data.Semigroup (stimesMonoid)

-- | Like 'on', but avoids recalculating values. Especially useful in folds.
on2 :: (a -> a -> c) -> (b -> a) -> b -> b -> c
(•) `on2` f = \x -> let fx = f x in (fx •) . f

-- | Apply a function `n` times.
--
-- > \x y -> y >= 0 ==> applyN y succ x == x + y
applyN :: Integral n => n -> (a -> a) -> (a -> a)
applyN n = appEndo . stimesMonoid n . Endo

-- | Apply a function if the condition succeeds.
--
-- > \x -> applyWhen True succ x == x + 1
-- > \b x -> applyWhen b succ x == applyN (fromEnum b) succ x
applyWhen :: Bool -> (a -> a) -> (a -> a)
applyWhen b f = if b then f else id

-- | Apply a single-argument function after a two-argument function.
--
-- > \x y -> (succ .: (+)) x y == (x + y) + 1
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .: f = (g .) . f
