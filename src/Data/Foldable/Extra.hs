module Data.Foldable.Extra
    ( sumOn'
    , productOn'
    ) where

import Data.Foldable

-- | A generalization of `Data.List.Extra.sum'` to Foldable instances.
sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl' (+) 0

-- | A generalization of `Data.List.Extra.product'` to Foldable instances.
product' :: (Foldable f, Num a) => f a -> a
product' = foldl' (*) 1

-- | A generalization of `Data.List.Extra.sumOn'` to Foldable instances.
sumOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn' f = foldl' (\acc x -> acc + f x) 0

-- | A generalization of `Data.List.Extra.productOn'` to Foldable instances.
productOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn' f = foldl' (\acc x -> acc * f x) 1
