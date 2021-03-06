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
    firstJust,
    firstJustM,
    foldMapM,
    compareLength,
    comparingLength,
    maximumOn,
    minimumOn,
) where

import qualified Control.Monad.Extra as MX
import Data.Foldable
import Data.Functor.Identity (Identity (..))
import Data.Tuple.Extra ((&&&))
import Partial (Partial)

-- | A composition of 'not' and 'null'.
--
-- > notNull []  == False
-- > notNull [1] == True
-- > \(xs :: [Int]) -> notNull xs == not (null xs)
notNull :: (Foldable f) => f a -> Bool
notNull = not . null

-- | A generalization of 'Data.List.Extra.sum'' to 'Foldable' instances.
--
-- > sum' [1, 2, 3] == 6
sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl' (+) 0

-- | A generalization of 'Data.List.Extra.product'' to 'Foldable' instances.
--
-- > product' [1, 2, 4] == 8
product' :: (Foldable f, Num a) => f a -> a
product' = foldl' (*) 1

-- | A generalization of 'Data.List.Extra.sumOn'' to 'Foldable' instances.
--
-- > sumOn' read ["1", "2", "3"] == 6
sumOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
sumOn' f = foldl' ((. f) . (+)) 0

-- | A generalization of 'Data.List.Extra.productOn'' to 'Foldable' instances.
--
-- > productOn' read ["1", "2", "4"] == 8
productOn' :: (Foldable f, Num b) => (a -> b) -> f a -> b
productOn' f = foldl' ((. f) . (*)) 1

-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > anyM Just [False,True ,undefined] == Just True
-- > anyM Just [False,False,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM p = foldr ((MX.||^) . p) (pure False)

-- | A version of 'all' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > allM Just [True,False,undefined] == Just False
-- > allM Just [True,True ,undefined] == undefined
-- > \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = foldr ((MX.&&^) . p) (pure True)

-- | A version of 'or' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > orM [Just False,Just True ,undefined] == Just True
-- > orM [Just False,Just False,undefined] == undefined
-- > \xs -> Just (or xs) == orM (map Just xs)
orM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
orM = anyM id

-- | A version of 'and' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > andM [Just True,Just False,undefined] == Just False
-- > andM [Just True,Just True ,undefined] == undefined
-- > \xs -> Just (and xs) == andM (map Just xs)
andM :: (Foldable f, Monad m) => f (m Bool) -> m Bool
andM = allM id

-- | Like 'find', but where the test can be monadic.
--
-- > findM (Just . isUpper) "teST"             == Just (Just 'S')
-- > findM (Just . isUpper) "test"             == Just Nothing
-- > findM (Just . const True) ["x",undefined] == Just (Just "x")
findM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p = foldr (MX.ifM <$> p <*> (pure . Just)) (pure Nothing)

-- | Find the first element of a `Foldable` for which the operation returns 'Just', along
--   with the result of the operation. Like `find` but useful where the function also
--   computes some expensive information that can be reused.
--
-- > firstJust id [Nothing,Just 3]  == Just 3
-- > firstJust id [Nothing,Nothing] == Nothing
firstJust :: (Foldable f) => (a -> Maybe b) -> f a -> Maybe b
firstJust p = runIdentity . firstJustM (Identity . p)

-- | Like 'findM', but also allows you to compute some additional information in the predicate.
--
-- > firstJustM (\a -> [if a > 0 then Just a else Nothing]) [-1, 0, 1] == [Just 1]
firstJustM :: (Foldable f, Monad m) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
firstJustM p = foldl' (\mmb a -> MX.maybeM (p a) (pure . Just) mmb) (pure Nothing)

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
