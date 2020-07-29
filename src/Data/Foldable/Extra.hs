module Data.Foldable.Extra where

import Data.Foldable

sumOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn' f = foldl' (\acc x -> acc + f x) 0

productOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn' f = foldl' (\acc x -> acc * f x) 1
