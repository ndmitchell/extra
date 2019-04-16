
-- | Extra functions for working with 'NonEmpty' lists. The package
--   also exports the existing "Data.List.NonEmpty" functions.
module Data.List.NonEmpty.Extra(
    module Data.List.NonEmpty,
    (|:), (|>), snoc,
    appendl, appendr
    ) where

import Data.List.NonEmpty

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
