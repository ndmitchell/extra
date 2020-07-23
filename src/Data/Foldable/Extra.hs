module Data.Foldable.Extra where

import Data.Foldable

sumOn :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn f = foldl' (\b a -> b + f a) 0

productOn :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn f = foldl' (\b a -> b * f a) 0
