{-# LANGUAGE CPP #-}

-- | Extra functions for working with 'NonEmpty' lists. The package
--   also exports the existing "Data.List.NonEmpty" functions.
module Data.List.NonEmpty.Extra (
    module Data.List.NonEmpty,
    (|:),
    (|>),
    snoc,
    appendl,
    appendr,
    sortOn,
    union,
    unionBy,
    nubOrd,
    nubOrdBy,
    nubOrdOn,
    maximum1,
    minimum1,
    maximumBy1,
    minimumBy1,
    maximumOn1,
    minimumOn1,
) where

import Data.Foldable.Extra (maximumBy, minimumBy)
import Data.Function (on)
import qualified Data.List.Extra as LX
import Data.List.NonEmpty

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
(|:) = flip (foldr cons . pure)

-- | Append a list to a non-empty list.
--
-- > appendl (1 :| [2,3]) [4,5] == 1 :| [2,3,4,5]
appendl :: NonEmpty a -> [a] -> NonEmpty a
appendl (x :| xs) l = x :| (xs ++ l)

-- | Append a non-empty list to a list.
--
-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr = flip (foldr cons)

-- | Sort by comparing the results of a function applied to each element.
--   The sort is stable, and the function is evaluated only once for
--   each element.
sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = fromList . LX.sortOn f . toList

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
nubOrdBy cmp = fromList . LX.nubOrdBy cmp . toList

-- | @nubOrdOn@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOrdOn'.
--
-- > Data.List.NonEmpty.Extra.nubOrdOn Data.List.length ("a" :| ["test","of","this"]) == "a" :| ["test","of"]
nubOrdOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
nubOrdOn f = fromList . LX.nubOrdOn f . toList

-- | The non-overloaded version of 'union'.
unionBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> NonEmpty a
unionBy eq = (fromList .) . (LX.unionBy eq `on` toList)

-- | The largest element of a non-empty list.
maximum1 :: Ord a => NonEmpty a -> a
maximum1 = maximum

-- | The least element of a non-empty list.
minimum1 :: Ord a => NonEmpty a -> a
minimum1 = minimum

-- | The largest element of a non-empty list with respect to the given
--   comparison function.
maximumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy1 = maximumBy

-- | The least element of a non-empty list with respect to the given
--   comparison function.
minimumBy1 :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy1 = minimumBy

-- | A version of 'maximum1' where the comparison is done on some extracted value.
maximumOn1 :: Ord b => (a -> b) -> NonEmpty a -> a
maximumOn1 = maximumBy1 . on compare

-- | A version of 'minimum1' where the comparison is done on some extracted value.
minimumOn1 :: Ord b => (a -> b) -> NonEmpty a -> a
minimumOn1 = minimumBy1 . on compare
