module Data.Foldable.Extra
    ( module Data.Foldable
    , sum'
    , product'
    , sumOn'
    , productOn'
    , anyM
    , allM
    , orM
    , andM
    , findM
    , firstJustM
    , compareLength
    , comparingLength
    ) where

import Data.Foldable
import qualified Control.Monad.Extra as MX

-- | A generalization of 'Data.List.Extra.sum'' to 'Foldable' instances.
sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl' (+) 0

-- | A generalization of 'Data.List.Extra.product'' to 'Foldable' instances.
product' :: (Foldable f, Num a) => f a -> a
product' = foldl' (*) 1

-- | A generalization of 'Data.List.Extra.sumOn'' to 'Foldable' instances.
sumOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn' f = foldl' (\acc x -> acc + f x) 0

-- | A generalization of 'Data.List.Extra.productOn'' to 'Foldable' instances.
productOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn' f = foldl' (\acc x -> acc * f x) 1

-- | A generalization of 'Control.Monad.Extra.anyM' to 'Foldable' instances. Retains the short-circuiting behaviour.
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr ((MX.||^) . p) (pure False)

-- | A generalization of 'Control.Monad.Extra.allM' to 'Foldable' instances. Retains the short-circuiting behaviour.
allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = foldr ((MX.&&^) . p) (pure True)

-- | A generalization of 'Control.Monad.Extra.orM' to 'Foldable' instances. Retains the short-circuiting behaviour.
orM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
orM = anyM id

-- | A generalization of 'Control.Monad.Extra.andM' to 'Foldable' instances. Retains the short-circuiting behaviour.
andM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
andM = allM id

-- | A generalization of 'Control.Monad.Extra.findM' to 'Foldable' instances.
findM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p = foldr (\x -> MX.ifM (p x) (pure $ Just x)) (pure Nothing)

-- | A generalization of 'Control.Monad.Extra.firstJustM' to 'Foldable' instances.
firstJustM :: (Foldable f, Monad m) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
firstJustM p = MX.firstJustM p . toList

-- | Lazily compare the length of a 'Foldable' with a number.
compareLength :: (Ord b, Num b, Foldable f) => f a -> b -> Ordering
compareLength = foldr (\_ acc n -> if n > 0 then acc (n - 1) else GT) (compare 0)

-- | Lazily compare the length of two 'Foldable's.
comparingLength :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Ordering
comparingLength x y = go (toList x) (toList y)
  where
    go [] [] = EQ
    go [] (_:_) = LT
    go (_:_) [] = GT
    go (_:xs) (_:ys) = go xs ys
