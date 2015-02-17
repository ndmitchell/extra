{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.List" with extra functions of a similar nature.
--   The package also exports the existing "Data.List" functions.
--   Some of the names and semantics were inspired by the
--   <http://hackage.haskell.org/package/text text> package.
module Data.List.Extra(
    module Data.List,
    -- * String operations
    lower, upper, trim, trimStart, trimEnd, word1,
    -- * Splitting    
    dropEnd, takeEnd, splitAtEnd, breakEnd, spanEnd,
    dropWhileEnd, dropWhileEnd', takeWhileEnd, stripSuffix,
    wordsBy, linesBy,
    breakOn, breakOnEnd, splitOn, split, chunksOf,
    -- * Basics
    list, uncons, unsnoc, cons, snoc, drop1,
    -- * List operations
    groupSort, groupSortOn, groupSortBy,
    nubOn, groupOn, sortOn,
    disjoint, allSame, anySame,
    repeatedly, for, firstJust,
    concatUnzip, concatUnzip3,
    replace, merge, mergeBy,
    ) where

import Data.List
import Data.Maybe
import Data.Function
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
-- > anySame (1:2:1:undefined) == True
-- > anySame [] == False
-- > \xs -> anySame xs == (length (nub xs) < length xs)
anySame :: Eq a => [a] -> Bool
anySame = f []
    where
        f seen (x:xs) = x `elem` seen || f (x:seen) xs
        f seen [] = False

-- | Are all elements the same.
--
-- > allSame [1,1,2] == False
-- > allSame [1,1,1] == True
-- > allSame [1]     == True
-- > allSame []      == True
-- > allSame (1:1:2:undefined) == False
-- > \xs -> allSame xs == (length (nub xs) <= 1)
allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x ==) xs


-- | Non-recursive transform over a list, like 'maybe'.
--
-- > list 1 (\v _ -> v - 2) [5,6,7] == 3
-- > list 1 (\v _ -> v - 2) []      == 1
-- > \nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs
list :: b -> (a -> [a] -> b) -> [a] -> b
list nil cons [] = nil
list nil cons (x:xs) = cons x xs

#if __GLASGOW_HASKELL__ < 709
-- | If the list is empty returns 'Nothing', otherwise returns the 'head' and the 'tail'.
--
-- > uncons "test" == Just ('t',"est")
-- > uncons ""     == Nothing
-- > \xs -> uncons xs == if null xs then Nothing else Just (head xs, tail xs)
uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)
#endif

-- | If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
--
-- > unsnoc "test" == Just ("tes",'t')
-- > unsnoc ""     == Nothing
-- > \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = Just (x:a, b)
    where Just (a,b) = unsnoc xs

-- | Append an element to the start of a list, an alias for '(:)'.
--
-- > cons 't' "est" == "test"
-- > \x xs -> uncons (cons x xs) == Just (x,xs)
cons :: a -> [a] -> [a]
cons = (:)

-- | Append an element to the end of a list, takes /O(n)/ time.
--
-- > snoc "tes" 't' == "test"
-- > \xs x -> unsnoc (snoc xs x) == Just (xs,x)
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]


-- | Take a number of elements from the end of the list.
--
-- > takeEnd 3 "hello"  == "llo"
-- > takeEnd 5 "bye"    == "bye"
-- > takeEnd (-1) "bye" == ""
-- > \i xs -> takeEnd i xs `isSuffixOf` xs
-- > \i xs -> length (takeEnd i xs) == min (max 0 i) (length xs)
takeEnd :: Int -> [a] -> [a]
takeEnd i xs = f xs (drop i xs)
    where f (x:xs) (y:ys) = f xs ys
          f xs _ = xs

-- | Drop a number of elements from the end of the list.
--
-- > dropEnd 3 "hello"  == "he"
-- > dropEnd 5 "bye"    == ""
-- > dropEnd (-1) "bye" == "bye"
-- > \i xs -> dropEnd i xs `isPrefixOf` xs
-- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
-- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
dropEnd :: Int -> [a] -> [a]
dropEnd i xs = f xs (drop i xs)
    where f (x:xs) (y:ys) = x : f xs ys
          f _ _ = []


-- | @'splitAtEnd' n xs@ returns a split where the second element tries to
--   contain @n@ elements.
--
-- > splitAtEnd 3 "hello" == ("he","llo")
-- > splitAtEnd 3 "he"    == ("", "he")
-- > \i xs -> uncurry (++) (splitAt i xs) == xs
-- > \i xs -> splitAtEnd i xs == (dropEnd i xs, takeEnd i xs)
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd i xs = f xs (drop i xs)
    where f (x:xs) (y:ys) = first (x:) $ f xs ys
          f xs _ = ([], xs)


-- | A merging of 'unzip' and 'concat'.
--
-- > concatUnzip [("a","AB"),("bc","C")] == ("abc","ABC")
concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . unzip

-- | A merging of 'unzip3' and 'concat'.
--
-- > concatUnzip3 [("a","AB",""),("bc","C","123")] == ("abc","ABC","123")
concatUnzip3 :: [([a],[b],[c])] -> ([a],[b],[c])
concatUnzip3 xs = (concat a, concat b, concat c)
    where (a,b,c) = unzip3 xs


-- | A version of 'takeWhile' operating from the end.
--
-- > takeWhileEnd even [2,3,4,6] == [4,6]
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = reverse . takeWhile f . reverse


-- | Remove spaces from the start of a string, see 'trim'.
trimStart :: String -> String
trimStart = dropWhile isSpace

-- | Remove spaces from the end of a string, see 'trim'.
trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- | Remove spaces from either side of a string. A combination of 'trimEnd' and 'trimStart'.
--
-- > trim      "  hello   " == "hello"
-- > trimStart "  hello   " == "hello   "
-- > trimEnd   "  hello   " == "  hello"
-- > \s -> trim s == trimEnd (trimStart s)
trim :: String -> String
trim = trimEnd . trimStart

-- | Convert a string to lower case.
--
-- > lower "This is A TEST" == "this is a test"
-- > lower "" == ""
lower :: String -> String
lower = map toLower

-- | Convert a string to upper case.
--
-- > upper "This is A TEST" == "THIS IS A TEST"
-- > upper "" == ""
upper :: String -> String
upper = map toUpper


-- | Split the first word off a string. Useful for when starting to parse the beginning
--   of a string, but you want to accurately perserve whitespace in the rest of the string.
--
-- > word1 "" == ("", "")
-- > word1 "keyword rest of string" == ("keyword","rest of string")
-- > word1 "  keyword\n  rest of string" == ("keyword","rest of string")
-- > \s -> fst (word1 s) == concat (take 1 $ words s)
-- > \s -> words (snd $ word1 s) == drop 1 (words s)
word1 :: String -> (String, String)
word1 x = second (dropWhile isSpace) $ break isSpace $ dropWhile isSpace x


#if __GLASGOW_HASKELL__ < 709
-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy (comparing f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- > sortOn fst [(3,"z"),(1,""),(3,"a")] == [(1,""),(3,"z"),(3,"a")]
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (compare `on` fst) . map (\x -> let y = f x in y `seq` (y, x))
#endif

-- | A version of 'group' where the equality is done on some extracted value.
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y


-- | A version of 'nub' where the equality is done on some extracted value.
--   @nubOn f@ is equivalent to @nubBy ((==) `on` f)@, but has the
--   performance advantage of only evaluating @f@ once for each element in the
--   input list.
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = map snd . nubBy ((==) `on` fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | A combination of 'group' and 'sort'.
--
-- > groupSort [(1,'t'),(3,'t'),(2,'e'),(2,'s')] == [(1,"t"),(2,"es"),(3,"t")]
-- > \xs -> map fst (groupSort xs) == sort (nub (map fst xs))
-- > \xs -> concatMap snd (groupSort xs) == map snd (sortOn fst xs)
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = map (\x -> (fst $ head x, map snd x)) . groupOn fst . sortOn fst

-- | A combination of 'group' and 'sort', using a part of the value to compare on.
--
-- > groupSortOn length ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map (f &&& id)

-- | A combination of 'group' and 'sort', using a predicate to compare on.
--
-- > groupSortBy (compare `on` length) ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\a b -> f a b == EQ) . sortBy f


-- | Merge two lists which are assumed to be ordered.
--
-- > merge "ace" "bd" == "abcde"
-- > \xs ys -> merge (sort xs) (sort ys) == sort (xs ++ ys)
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare


-- | Like 'merge', but with a custom ordering function.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f xs [] = xs
mergeBy f [] ys = ys
mergeBy f (x:xs) (y:ys)
    | f x y /= GT = x : mergeBy f xs (y:ys)
    | otherwise = y : mergeBy f (x:xs) ys


-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- > replace "el" "_" "Hello Bella" == "H_lo B_la"
-- > replace "el" "e" "Hello"       == "Helo"
-- > replace "" "e" "Hello"         == undefined
-- > \xs ys -> not (null xs) ==> replace xs xs ys == ys
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []


-- | Break, but from the end.
--
-- > breakEnd isLower "youRE" == ("you","RE")
-- > breakEnd isLower "youre" == ("youre","")
-- > breakEnd isLower "YOURE" == ("","YOURE")
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd f = swap . both reverse . break f . reverse

-- | Span, but from the end.
--
-- > spanEnd isUpper "youRE" == ("you","RE")
-- > spanEnd (not . isSpace) "x y z" == ("x y ","z")
-- > \f xs-> spanEnd f xs == swap (both reverse (span f (reverse xs)))
spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f = breakEnd (not . f)


-- | A variant of 'words' with a custom test. In particular,
--   adjacent separators are discarded, as are leading or trailing separators.
--
-- > wordsBy (== ':') "::xyz:abc::123::" == ["xyz","abc","123"]
-- > \s -> wordsBy isSpace s == words s
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    x:xs -> (x:w) : wordsBy f (drop1 z)
        where (w,z) = break f xs

-- | A variant of 'lines' with a custom test. In particular,
--   if there is a trailing separator it will be discarded.
--
-- > linesBy (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123",""]
-- > \s -> linesBy (== '\n') s == lines s
-- > linesBy (== ';') "my;list;here;" == ["my","list","here"]
linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy f [] = []
linesBy f s = cons $ case break f s of
    (l, s) -> (l,) $ case s of
        [] -> []
        _:s -> linesBy f s
  where
    cons ~(h, t) = h : t -- to fix a space leak, see the GHC defn of lines

-- | Find the first element of a list for which the operation returns 'Just', along
--   with the result of the operation. Like 'find' but useful where the function also
--   computes some expensive information that can be reused. Particular useful
--   when the function is monadic, see 'firstJustM'.
--
-- > firstJust id [Nothing,Just 3]  == Just 3
-- > firstJust id [Nothing,Nothing] == Nothing
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f


-- | Equivalent to @drop 1@, but likely to be faster and a single lexeme.
--
-- > drop1 ""         == ""
-- > drop1 "test"     == "est"
-- > \xs -> drop 1 xs == drop1 xs
drop1 :: [a] -> [a]
drop1 [] = []
drop1 (x:xs) = xs


-- | Find the first instance of @needle@ in @haystack@.
-- The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- > breakOn "::" "a::b::c" == ("a", "::b::c")
-- > breakOn "/" "foobar"   == ("foobar", "")
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
-- > splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > splitOn "x"    "x"                == ["",""]
-- > splitOn "x"    ""                 == [""]
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
-- separators result in an empty component in the output.
--
-- > split (== 'a') "aabbaca" == ["","","bb","c",""]
-- > split (== 'a') ""        == [""]
-- > split (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123","",""]
-- > split (== ',') "my,list,here" == ["my","list","here"]
split :: (a -> Bool) -> [a] -> [[a]]
split f [] = [[]]
split f (x:xs) | f x = [] : split f xs
split f (x:xs) | y:ys <- split f xs = (x:y) : ys


#if __GLASGOW_HASKELL__ < 704
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
#endif


-- | A version of 'dropWhileEnd' but with different strictness properties.
--   The function 'dropWhileEnd' can be used on an infinite list and tests the property
--   on each character. In contrast, 'dropWhileEnd'' is strict in the spine of the list
--   but only tests the trailing suffix.
--   This version usually outperforms 'dropWhileEnd' if the list is short or the test is expensive.
--   Note the tests below cover both the prime and non-prime variants.
--
-- > dropWhileEnd  isSpace "ab cde  " == "ab cde"
-- > dropWhileEnd' isSpace "ab cde  " == "ab cde"
-- > last (dropWhileEnd  even [undefined,3]) == undefined
-- > last (dropWhileEnd' even [undefined,3]) == 3
-- > head (dropWhileEnd  even (3:undefined)) == 3
-- > head (dropWhileEnd' even (3:undefined)) == undefined
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
-- > chunksOf 0 "test"    == undefined
chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
chunksOf i xs = repeatedly (splitAt i) xs
