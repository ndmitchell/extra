module Data.Foldable.Extra (
    module Data.Foldable,
    notNull,
    sum',
    product',
    sumOn',
    productOn',
    anyM,
    allM,
    orM,
    andM,
    findM,
    firstJustM,
    foldMapM,
    compareLength,
    comparingLength,
    maximumOn,
    minimumOn,
) where

import qualified Control.Monad.Extra as MX
import Data.Foldable
import Data.Tuple.Extra ((&&&))
import Partial (Partial)

-- | A composition of 'not' and 'null'.
--
-- > notNull []  == False
-- > notNull [1] == True
-- > \xs -> notNull xs == not (null xs)
notNull :: (Foldable f) => f a -> Bool
notNull = not . null

-- | A generalization of 'Data.List.Extra.sum'' to 'Foldable' instances.
sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl' (+) 0

-- | A generalization of 'Data.List.Extra.product'' to 'Foldable' instances.
product' :: (Foldable f, Num a) => f a -> a
product' = foldl' (*) 1

-- | A generalization of 'Data.List.Extra.sumOn'' to 'Foldable' instances.
sumOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn' f = foldl' ((. f) . (+)) 0

-- | A generalization of 'Data.List.Extra.productOn'' to 'Foldable' instances.
productOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn' f = foldl' ((. f) . (*)) 1

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
findM p = foldr (MX.ifM <$> p <*> (pure . Just)) (pure Nothing)

-- | A generalization of 'Control.Monad.Extra.firstJustM' to 'Foldable' instances.
firstJustM :: (Foldable f, Monad m) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
firstJustM p = MX.firstJustM p . toList

-- | A generalization of 'Control.Monad.Extra.mconcatMapM' to 'Traversable' instances.
foldMapM :: (Traversable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM = (fmap fold .) . traverse

-- | Lazily compare the length of a 'Foldable' with a number.
--
-- > compareLength [1,2,3] 1 == GT
-- > compareLength [1,2] 2 == EQ
-- > \(xs :: [Int]) n -> compareLength xs n == compare (length xs) n
-- > compareLength (1:2:3:undefined) 2 == GT
compareLength :: (Ord b, Num b, Foldable f) => f a -> b -> Ordering
compareLength = foldr (\_ acc n -> if n > 0 then acc (n - 1) else GT) (compare 0)

-- | Lazily compare the length of two 'Foldable's.
-- > comparingLength [1,2,3] [False] == GT
-- > comparingLength [1,2] "ab" == EQ
-- > \(xs :: [Int]) (ys :: [Int]) -> comparingLength xs ys == Data.Ord.comparing length xs ys
-- > comparingLength [1,2] (1:2:3:undefined) == LT
-- > comparingLength (1:2:3:undefined) [1,2] == GT
comparingLength :: (Foldable f1, Foldable f2) => f1 a -> f2 b -> Ordering
comparingLength x y = go (toList x) (toList y)
  where
    go [] [] = EQ
    go [] (_ : _) = LT
    go (_ : _) [] = GT
    go (_ : xs) (_ : ys) = go xs ys

-- | A version of 'maximum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > maximumOn id [] == undefined
-- > maximumOn length ["test","extra","a"] == "extra"
maximumOn :: (Partial, Functor f, Foldable f, Ord b) => (a -> b) -> f a -> a
maximumOn f = snd . getFst . maximum . fmap (Fst . (f &&& id))

-- | A version of 'minimum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > minimumOn id [] == undefined
-- > minimumOn length ["test","extra","a"] == "a"
minimumOn :: (Partial, Functor f, Foldable f, Ord b) => (a -> b) -> f a -> a
minimumOn f = snd . getFst . minimum . fmap (Fst . (f &&& id))

newtype Fst a b = Fst {getFst :: (a, b)}

instance Eq a => Eq (Fst a b) where
    Fst (a, _) == Fst (b, _) = a == b

instance Ord a => Ord (Fst a b) where
    compare (Fst (a, _)) (Fst (b, _)) = compare a b
