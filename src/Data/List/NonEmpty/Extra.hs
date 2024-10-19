{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Extra functions for working with 'NonEmpty' lists. The package
--   also exports the existing "Data.List.NonEmpty" functions.
module Data.List.NonEmpty.Extra(
    module Data.List.NonEmpty,
    (|:), (|>), snoc, (!?),
    appendl, appendr,
    sortOn, union, unionBy,
    nubOrd, nubOrdBy, nubOrdOn,
    maximum1, minimum1, maximumBy1, minimumBy1, maximumOn1, minimumOn1,
    foldl1', repeatedly,
    compareLength
    ) where

import           Data.Function
import qualified Data.List.Extra as List
import           Data.List.NonEmpty

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

-- | A total variant of the list index function `(!?)`.
--
-- > (2 :| [3,4]) !? 1    == Just 3
-- > (2 :| [3,4]) !? (-1) == Nothing
-- > (1 :| [])    !? 1    == Nothing
(!?) :: NonEmpty a -> Int -> Maybe a
(!?) ~(x :| xs) n
  | n == 0 = Just x
  | n > 0  = xs List.!? (n - 1)
  | otherwise = Nothing
infixl 9 !?

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

#if __GLASGOW_HASKELL__ <= 908
-- | Sort by comparing the results of a function applied to each element.
--   The sort is stable, and the function is evaluated only once for
--   each element.
sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = fromList . List.sortOn f . toList
#endif

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
nubOrdBy cmp = fromList . List.nubOrdBy cmp . toList

-- | @nubOrdOn@ for 'NonEmpty'. Behaves the same as 'Data.List.Extra.nubOrdOn'.
--
-- > Data.List.NonEmpty.Extra.nubOrdOn Data.List.length ("a" :| ["test","of","this"]) == "a" :| ["test","of"]
nubOrdOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
nubOrdOn f = fromList . List.nubOrdOn f . toList

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

-- | A strict variant of variant `foldl1`
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (x:|xs) =  List.foldl' f x xs

-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
repeatedly :: (NonEmpty a -> (b, [a])) -> NonEmpty a -> NonEmpty b
repeatedly f (a :| as) = b :| List.repeatedlyNE f as'
    where (b, as') = f (a :| as)

#if __GLASGOW_HASKELL__ <= 910
-- | Use 'compareLength' @xs@ @n@ as a safer and faster alternative
-- to 'compare' ('length' @xs@) @n@. Similarly, it's better
-- to write @compareLength xs 10 == LT@ instead of @length xs < 10@.
--
-- While 'length' would force and traverse
-- the entire spine of @xs@ (which could even diverge if @xs@ is infinite),
-- 'compareLength' traverses at most @n@ elements to determine its result.
--
-- >>> compareLength ('a' :| []) 1
-- EQ
-- >>> compareLength ('a' :| ['b']) 3
-- LT
-- >>> compareLength (0 :| [1..]) 100
-- GT
-- >>> compareLength undefined 0
-- GT
-- >>> compareLength ('a' :| 'b' : undefined) 1
-- GT
--
-- @since 4.21.0.0
--
compareLength :: NonEmpty a -> Int -> Ordering
compareLength xs n
  | n < 1 = GT
  | otherwise = foldr
    (\_ f m -> if m > 0 then f (m - 1) else GT)
    (\m -> if m > 0 then LT else EQ)
    xs
    n
#endif
