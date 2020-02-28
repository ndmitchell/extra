{-# LANGUAGE CPP #-}

-- | Extra functions for working with 'NonEmpty' lists. The package
--   also exports the existing "Data.List.NonEmpty" functions.
module Data.List.NonEmpty.Extra(
    module Data.List.NonEmpty,
    (|:), (|>), snoc,
    appendl, appendr,
    sortOn, union, unionBy,
    nubOrd, nubOrdBy, nubOrdOn, nubOn,
    maximum1, minimum1, maximumBy1, minimumBy1, maximumOn1, minimumOn1
    ) where

import           Data.Function
import qualified Data.List.Extra as List
import           Data.List.NonEmpty
import           Data.Tuple.Extra

#if __GLASGOW_HASKELL__ <= 802
import Data.Semigroup ((<>))
#endif

infixl 5 |>, |:

-- | /O(n)/. Append an element to a non-empty list.
--
-- > (1 :| [2,3]) |> 4 |> 5 == 1 :| [2,3,4,5]
(|>) :: NonEmpty a -> a -> NonEmpty a
(|>) xs x = xs <> pure x

-- | Synonym for '|>'.
snoc :: NonEmpty a -> a -> NonEmpty a
snoc = (|>)

-- | /O(n)/. Append an element to a list.
--
-- > [1,2,3] |: 4 |> 5 == 1 :| [2,3,4,5]
(|:) :: [a] -> a -> NonEmpty a
(|:) xs x = foldr cons (pure x) xs

-- | Append a list to a non-empty list.
--
-- > appendl (1 :| [2,3]) [4,5] == 1 :| [2,3,4,5]
appendl :: NonEmpty a -> [a] -> NonEmpty a
appendl (x :| xs) l = x :| (xs ++ l)

-- | Append a non-empty list to a list.
--
-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr l nel = foldr cons nel l

-- | Sort by comparing the results of a function applied to each element.
--   The sort is stable, and the function is evaluated only once for
--   each element.
sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = fromList . List.sortOn f . toList

-- | Return the union of two non-empty lists. Duplicates, and elements of the
--   first list, are removed from the the second list, but if the first list
--   contains duplicates, so will the result.
--
-- > (1 :| [3, 5, 3]) `union` (4 :| [5, 3, 5, 2]) == 1 :| [3, 5, 3, 4, 2]
union :: Eq a => NonEmpty a -> NonEmpty a -> NonEmpty a
union = unionBy (==)

-- | @nubOrd@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOrd'.
--
-- > Data.List.NonEmpty.Extra.nubOrd (1 :| [2, 3, 3, 4, 1, 2]) == 1 :| [2, 3, 4]
-- > \xs -> Data.List.NonEmpty.Extra.nubOrd xs == Data.List.NonEmpty.Extra.nub xs
nubOrd :: Ord a => NonEmpty a -> NonEmpty a
nubOrd = nubOrdBy compare

-- | @nubOrdBy@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOrdBy'.
--
-- > Data.List.NonEmpty.Extra.nubOrdBy (compare `on` Data.List.length) ("a" :| ["test","of","this"]) == "a" :| ["test","of"]
nubOrdBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
nubOrdBy cmp (x :| xs) = x :| List.nubOrdBy cmp (List.filter (\y -> cmp x y /= EQ) xs)

-- | @nubOrdOn@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOrdOn'.
--
-- > Data.List.NonEmpty.Extra.nubOrdOn Data.List.length ("a" :| ["test","of","this"]) == "a" :| ["test","of"]
nubOrdOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
nubOrdOn f = fmap snd . nubOrdBy (compare `on` fst) . fmap (f &&& id)

-- | @nubOn@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOn'.
nubOn :: Eq b => (a -> b) -> NonEmpty a -> NonEmpty a
nubOn f = fmap snd . nubBy ((==) `on` fst) . fmap (\x -> let y = f x in y `seq` (y, x))

-- | The non-overloaded version of 'union'.
unionBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionBy eq xs ys = fromList $ List.unionBy eq (toList xs) (toList ys)

-- | The largest element of a non-empty list.
maximum1 :: Ord a => NonEmpty a -> a
maximum1 = List.maximum

-- | The least element of a non-empty list.
minimum1 :: Ord a => NonEmpty a -> a
minimum1 = List.minimum

-- | The largest element of a non-empty list with respect to the given
--   comparison function.
maximumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy1 = List.maximumBy

-- | The least element of a non-empty list with respect to the given
--   comparison function.
minimumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy1 = List.minimumBy

-- | A version of 'maximum1' where the comparison is done on some extracted value.
maximumOn1 :: Ord b => (a -> b) -> NonEmpty a -> a
maximumOn1 f = maximumBy1 (compare `on` f)

-- | A version of 'minimum1' where the comparison is done on some extracted value.
minimumOn1 :: Ord b => (a -> b) -> NonEmpty a -> a
minimumOn1 f = minimumBy1 (compare `on` f)
