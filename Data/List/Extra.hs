
-- FIXME: Todo

-- | This module extends "Data.List" with extra functions of a similar nature.
--   The package also exports the existing "Data.List" functions.
module Data.List.Extra(
    module Data.List,
    module Data.List.Extra
    ) where

import Data.List
import Data.Function
import Data.Ord
import Control.Arrow
import Data.Char
import Data.Maybe


chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
    where (b, as') = f as

rep :: Eq a => a -> a -> a -> a
rep from to x = if x == from then to else x

reps :: Eq a => a -> a -> [a] -> [a]
reps from to = map (rep from to)


unzipEithers :: [Either a b] -> ([a],[b])
unzipEithers [] = ([],[])
unzipEithers (Left x:xs) = (x:a,b)
    where (a,b) = unzipEithers xs
unzipEithers (Right x:xs) = (a,x:b)
    where (a,b) = unzipEithers xs


initLast :: [a] -> ([a], a)
initLast [] = error "initLast, empty list []"
initLast [x] = ([], x)
initLast (x:xs) = (x:a, b)
    where (a,b) = initLast xs

for = flip map

notNull = not . null

groupSortFst :: Ord a => [(a,b)] -> [(a,[b])]
groupSortFst = map (fst . head &&& map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs

unsnoc :: [a] -> ([a],a)
unsnoc [] = error "Unsnoc on empty list"
unsnoc xs = (init xs, last xs)

revTake :: Int -> [a] -> [a]
revTake i = reverse . take i . reverse

concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . unzip


replace :: String -> String -> String -> String
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []




trimLeft = dropWhile isSpace
trimRight = reverse . trimLeft . reverse
trim = trimLeft . trimRight



trim, trimLeft, trimRight :: String -> String

lower = map toLower
upper = map toUpper

trimBy :: (a -> Bool) -> [a] -> [a]
trimBy f = reverse . dropWhile f . reverse . dropWhile f


word1 :: String -> (String, String)
word1 x = second (dropWhile isSpace) $ break isSpace $ dropWhile isSpace x


-- | Only append strings if neither one is empty
(++?) :: String -> String -> String
a ++? b = if null a || null b then [] else a ++ b

sortOn f = sortBy (comparing f)
groupOn f = groupBy ((==) `on` f)
nubOn f = nubBy ((==) `on` f)

sortFst mr = sortOn fst mr
groupFst mr = groupOn fst mr


groupFsts :: Eq k => [(k,v)] -> [(k,[v])]
groupFsts = map (fst . head &&& map snd) . groupFst

sortGroupFsts mr = groupFsts . sortFst $ mr
sortGroupFst mr = groupFst . sortFst $ mr


fold :: a -> (a -> a -> a) -> [a] -> a
fold x f [] = x
fold x f xs = fold1 f xs


fold1 :: (a -> a -> a) -> [a] -> a
fold1 f [x] = x
fold1 f xs = f (fold1 f a) (fold1 f b)
    where (a,b) = halves xs


halves :: [a] -> ([a],[a])
halves [] = ([], [])
halves (x:xs) = (x:b,a)
    where (a,b) = halves xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys)
    | f x y /= GT = x : mergeBy f xs (y:ys)
    | otherwise = y : mergeBy f (x:xs) ys


merges :: Ord a => [[a]] -> [a]
merges = fold [] merge

mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesBy f = fold [] (mergeBy f)


split :: Eq a => a -> [a] -> [[a]]
split x [] = []
split x xs = if null b then [a] else a : split x (tail b)
    where (a,b) = break (== x) xs



-- | Like splitAt, but also return the number of items that were split.
--   For performance.
splitAtLength :: Int -> [a] -> (Int,[a],[a])
splitAtLength n xs = f n xs
    where
        f i xs | i == 0 = (n,[],xs)
        f i [] = (n-i,[],[])
        f i (x:xs) = (a,x:b,c)
            where (a,b,c) = f (i-1) xs


rbreak f xs = case break f $ reverse xs of
    (_, []) -> (xs, [])
    (as, b:bs) -> (reverse bs, b:reverse as)




splitList :: Eq a => [a] -> [a] -> [[a]]
splitList find str = if isJust q then a : splitList find b else [str]
    where
        q = splitPair find str
        Just (a, b) = q


splitPair :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitPair find str = f str
    where
        f [] = Nothing
        f x  | isPrefixOf find x = Just ([], drop (length find) x)
             | otherwise = if isJust q then Just (head x:a, b) else Nothing
                where
                    q = f (tail x)
                    Just (a, b) = q

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    x:xs -> (x:w) : wordsBy f (drop1 z)
        where (w,z) = break f xs

linesBy = undefined

findJust = undefined

drop1 = drop 1


splitx :: [a] -> [a] -> [[a]]
splitx = undefined

{-
dropWhileEnd :: (a -> Bool) -> [a] -> [a] Source
dropWhileEnd = undefined

The dropWhileEnd function drops the largest suffix of a list in which the given predicate holds for all elements. For example:

dropWhileEnd isSpace "foo\n" == "foo"
dropWhileEnd isSpace "foo bar" == "foo bar"
dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
Since: 4.5.0.0
-}

{-
findJust
-}

list :: b -> (a -> [a] -> b) -> b
list = undefined
