{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.List" with extra functions of a similar nature.
--   The package also exports the existing "Data.List" functions.
--   Some of the names and semantics were inspired by the @text@ package.
module Data.List.Extra(
    module Data.List,
    lower, upper, trim, trimStart, trimEnd, dropAround, word1, drop1,
    list, uncons, unsnoc, cons, snoc,
    groupSort, groupSortOn, nubOn, groupOn, sortOn,
    repeatedly, for,
    disjoint, allSame, anySame,
    dropEnd, takeEnd, breakEnd, spanEnd, dropWhileEnd, dropWhileEnd', takeWhileEnd, stripSuffix,
    concatUnzip, concatUnzip3,
    merge, mergeBy, replace, wordsBy, linesBy, firstJust,
    breakOn, breakOnEnd, splitOn, split, chunksOf
    ) where

import Data.List
import Data.Function
import Data.Ord
import Data.Char
import Data.Tuple.Extra


-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
--
-- > \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
-- > \xs -> repeatedly word1 (trim xs) == words xs
repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as


-- | Flipped version of 'map'.
--
-- > for [1,2,3] (+1) == [2,3,4]
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Are two lists disjoint, with no elements in common.
--
-- > disjoint [1,2,3] [4,5] == True
-- > disjoint [1,2,3] [4,1] == False
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs

-- | Is there any element which occurs more than once.
--
-- > anySame [1,1,2] == True
-- > anySame [1,2,3] == False
anySame :: Eq a => [a] -> Bool
anySame xs = length xs /= length (nub xs)

-- | Are all elements the same.
--
-- > allSame [1,1,2] == False
-- > allSame [1,1,1] == True
-- > allSame [1]     == True
-- > allSame []      == True
allSame :: Eq a => [a] -> Bool
allSame xs = length (nub xs) <= 1


list :: b -> (a -> [a] -> b) -> [a] -> b
list nil cons [] = nil
list nil cons (x:xs) = cons x xs

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = Just (x:a, b)
    where Just (a,b) = unsnoc xs

cons :: a -> [a] -> [a]
cons = (:)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

takeEnd :: Int -> [a] -> [a]
takeEnd i = reverse . take i . reverse

dropEnd :: Int -> [a] -> [a]
dropEnd i = reverse . drop i . reverse

concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . unzip

concatUnzip3 :: [([a],[b],[c])] -> ([a],[b],[c])
concatUnzip3 xs = (concat a, concat b, concat c)
    where (a,b,c) = unzip3 xs


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = reverse . takeWhile f . reverse


trim, trimStart, trimEnd :: String -> String
trimStart = dropWhile isSpace
trimEnd = dropWhileEnd isSpace
trim = dropAround isSpace

-- | Documentation about lowercase
--
-- > lower "This is A TEST" == "this is a test"
-- > lower "" == ""
lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper

dropAround :: (a -> Bool) -> [a] -> [a]
dropAround f = dropWhileEnd f . dropWhile f


word1 :: String -> (String, String)
word1 x = second (dropWhile isSpace) $ break isSpace $ dropWhile isSpace x


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = groupSortOn id

groupSortOn :: Ord a => (k -> a) -> [(k, v)] -> [(k, [v])]
groupSortOn f = map (\x -> (fst $ head x, map snd x)) . groupOn (f . fst) . sortOn (f . fst)


merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare


mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys)
    | f x y /= GT = x : mergeBy f xs (y:ys)
    | otherwise = y : mergeBy f (x:xs) ys

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []


breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd f xs = case break f $ reverse xs of
    (_, []) -> (xs, [])
    (as, b:bs) -> (reverse bs, b:reverse as)

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f xs = breakEnd (not . f) xs


wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    x:xs -> (x:w) : wordsBy f (drop1 z)
        where (w,z) = break f xs

linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy f [] = []
linesBy f s = cons $ case break f s of
    (l, s) -> (l,) $ case s of
        [] -> []
        _:s -> linesBy f s
  where
    cons ~(h, t) = h : t -- to fix a space leak, see the GHC defn of lines

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust p [] = Nothing
firstJust p (x:xs) = maybe (firstJust p xs) Just (p x)

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (x:xs) = xs


-- | Find the first instance of @needle@ in @haystack@.
-- The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- > breakOn "::" "a::b::c" == ("a", "::b::c")
-- > breakOn "/" "foobar"   == ("foobar", "")
--
-- Laws:
--
-- > \needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack
breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `isPrefixOf` haystack = ([], haystack)
breakOn needle [] = ([], [])
breakOn needle (x:xs) = first (x:) $ breakOn needle xs

-- | Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- > breakOnEnd "::" "a::b::c" == ("a::b::", "c")
breakOnEnd :: Eq a => [a] -> [a] -> ([a], [a])
breakOnEnd needle haystack = (reverse *** reverse) $ swap $ breakOn (reverse needle) (reverse haystack)


-- | Break a list into pieces separated by the first
-- list argument, consuming the delimiter. An empty delimiter is
-- invalid, and will cause an error to be raised.
--
-- Examples:
--
-- > splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > splitOn "x"    "x"                == ["",""]
-- > splitOn "x"    ""                 == [""]
--
-- and
--
-- > \s x -> s /= "" ==> intercalate s (splitOn s x) == x
-- > \c x -> splitOn [c] x                           == split (==c) x
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn [] _ = error "splitOn, needle may not be empty"
splitOn _ [] = [[]]
splitOn needle haystack = a : if null b then [] else splitOn needle $ drop (length needle) b
    where (a,b) = breakOn needle haystack


-- | Splits a list into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > split (=='a') "aabbaca" == ["","","bb","c",""]
-- > split (=='a') ""        == [""]
split :: (a -> Bool) -> [a] -> [[a]]
split f [] = [[]]
split f (x:xs) | f x = [] : split f xs
split f (x:xs) | y:ys <- split f xs = (x:y) : ys


#if __GLASGOW_HASKELL__ < 704
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
#endif


-- | A version of 'dropWhileEnd' but with different strictness properties.
--   Often outperforms if the list is short or the test is expensive.
dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = foldr (\x xs -> if null xs && p x then [] else x : xs) []

-- | Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- > stripSuffix "bar" "foobar" == Just "foo"
-- > stripSuffix ""    "baz"    == Just "baz"
-- > stripSuffix "foo" "quux"   == Nothing
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = fmap reverse $ stripPrefix (reverse a) (reverse b)


-- | Split a list into chunks of a given size. The last chunk may contain
--   fewer than n elements. The chunk size must be positive.
--
-- > chunksOf 3 "my test" == ["my ","tes","t"]
-- > chunksOf 3 "mytest"  == ["myt","est"]
-- > chunksOf 8 ""        == []
-- > chunksOf 0 "test"    == error
chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
chunksOf i xs = repeatedly (splitAt i) xs
