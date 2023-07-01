{-# LANGUAGE CPP, TupleSections, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.List" with extra functions of a similar nature.
--   The package also exports the existing "Data.List" functions.
--   Some of the names and semantics were inspired by the
--   <https://hackage.haskell.org/package/text text> package.
module Data.List.Extra(
    module Data.List,
    -- * String operations
    lower, upper, trim, trimStart, trimEnd, word1, line1,
    escapeHTML, escapeJSON,
    unescapeHTML, unescapeJSON,
    -- * Splitting
    dropEnd, takeEnd, splitAtEnd, breakEnd, spanEnd,
    dropWhileEnd', takeWhileEnd,
    stripSuffix, stripInfix, stripInfixEnd,
    dropPrefix, dropSuffix,
    wordsBy, linesBy,
    breakOn, breakOnEnd, splitOn, split, chunksOf,
    -- * Basics
    headDef, lastDef, (!?), notNull, list, unsnoc, cons, snoc,
    drop1, dropEnd1, mconcatMap, compareLength, comparingLength,
    -- * Enum operations
    enumerate,
    -- * List operations
    groupSort, groupSortOn, groupSortBy,
    nubOrd, nubOrdBy, nubOrdOn,
    nubOn, groupOn, groupOnKey,
    nubSort, nubSortBy, nubSortOn,
    maximumOn, minimumOn,
    sum', product',
    sumOn', productOn',
    disjoint, disjointOrd, disjointOrdBy, allSame, anySame,
    repeatedly, repeatedlyNE, firstJust,
    concatUnzip, concatUnzip3,
    zipFrom, zipWithFrom, zipWithLongest,
    replace, merge, mergeBy,
    ) where

import Partial
import Data.List
import Data.Maybe
import Data.Function
import Data.Char
import Data.Tuple.Extra
import Data.Monoid
import Numeric
import Data.Functor
import Data.Foldable
import Prelude
import Data.List.NonEmpty (NonEmpty ((:|)))


-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
--
-- When the empty list is reached it is returned, so the operation
-- is /never/ applied to the empty input.
-- That fact is encoded in the type system with 'repeatedlyNE'
--
-- > \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
-- > \xs -> repeatedly word1 (trim xs) == words xs
-- > \xs -> repeatedly line1 xs == lines xs
repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as

-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
--
-- Identical to 'repeatedly', but has a more precise type signature.
repeatedlyNE :: (NonEmpty a -> (b, [a])) -> [a] -> [b]
repeatedlyNE f [] = []
repeatedlyNE f (a : as) = b : repeatedlyNE f as'
    where (b, as') = f (a :| as)



-- | Are two lists disjoint, with no elements in common.
--
-- > disjoint [1,2,3] [4,5] == True
-- > disjoint [1,2,3] [4,1] == False
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs

-- | /O((m+n) log m), m <= n/. Are two lists disjoint, with no elements in common.
--
-- @disjointOrd@ is more strict than `disjoint`. For example, @disjointOrd@ cannot
-- terminate if both lists are infinite, while `disjoint` can.
--
-- > disjointOrd [1,2,3] [4,5] == True
-- > disjointOrd [1,2,3] [4,1] == False
disjointOrd :: Ord a => [a] -> [a] -> Bool
disjointOrd = disjointOrdBy compare

-- | A version of 'disjointOrd' with a custom predicate.
--
-- > disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,5] == True
-- > disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,8] == False
disjointOrdBy :: (a -> a -> Ordering) -> [a] -> [a] -> Bool
disjointOrdBy cmp xs ys
    | shorter xs ys = go xs ys
    | otherwise = go ys xs
  where
    shorter _ [] = False
    shorter [] _ = True
    shorter (_:xs) (_:ys) = shorter xs ys

    go xs = not . any (\a -> memberRB cmp a tree)
      where
        tree = foldl' (flip (insertRB cmp)) E xs

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


-- | A total 'head' with a default value.
--
-- > headDef 1 []      == 1
-- > headDef 1 [2,3,4] == 2
-- > \x xs -> headDef x xs == fromMaybe x (listToMaybe xs)
headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x:_) = x


-- | A total 'last' with a default value.
--
-- > lastDef 1 []      == 1
-- > lastDef 1 [2,3,4] == 4
-- > \x xs -> lastDef x xs == last (x:xs)
lastDef :: a -> [a] -> a
lastDef d xs = foldl (\_ x -> x) d xs -- I know this looks weird, but apparently this is the fastest way to do this: https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#last
{-# INLINE lastDef #-}

#if __GLASGOW_HASKELL__ <= 906
-- | A total variant of the list index function `(!!)`.
--
-- > [2,3,4] !? 1    == Just 3
-- > [2,3,4] !? (-1) == Nothing
-- > []      !? 0    == Nothing
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
             -- Definition adapted from GHC.List
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
{-# INLINABLE (!?) #-}
#endif

-- | A composition of 'not' and 'null'.
--
-- > notNull []  == False
-- > notNull [1] == True
-- > \xs -> notNull xs == not (null xs)
notNull :: [a] -> Bool
notNull = not . null

-- | Non-recursive transform over a list, like 'maybe'.
--
-- > list 1 (\v _ -> v - 2) [5,6,7] == 3
-- > list 1 (\v _ -> v - 2) []      == 1
-- > \nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs
list :: b -> (a -> [a] -> b) -> [a] -> b
list nil cons [] = nil
list nil cons (x:xs) = cons x xs

#if __GLASGOW_HASKELL__ <= 906
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
#endif

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


-- | Enumerate all the values of an 'Enum', from 'minBound' to 'maxBound'.
--
-- > enumerate == [False, True]
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound..maxBound]

-- | Take a number of elements from the end of the list.
--
-- > takeEnd 3 "hello"  == "llo"
-- > takeEnd 5 "bye"    == "bye"
-- > takeEnd (-1) "bye" == ""
-- > \i xs -> takeEnd i xs `isSuffixOf` xs
-- > \i xs -> length (takeEnd i xs) == min (max 0 i) (length xs)
takeEnd :: Int -> [a] -> [a]
takeEnd i xs
    | i <= 0 = []
    | otherwise = f xs (drop i xs)
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
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
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
splitAtEnd i xs
    | i <= 0 = (xs, [])
    | otherwise = f xs (drop i xs)
    where f (x:xs) (y:ys) = first (x:) $ f xs ys
          f xs _ = ([], xs)


-- | 'zip' against an enumeration.
--   Truncates the output if the enumeration runs out.
--
-- > \i xs -> zip [i..] xs == zipFrom i xs
-- > zipFrom False [1..3] == [(False,1),(True, 2)]
zipFrom :: Enum a => a -> [b] -> [(a, b)]
zipFrom = zipWithFrom (,)

-- | 'zipFrom' generalised to any combining operation.
--   Truncates the output if the enumeration runs out.
--
-- > \i xs -> zipWithFrom (,) i xs == zipFrom i xs
zipWithFrom :: Enum a => (a -> b -> c) -> a -> [b] -> [c]
-- would love to deforest the intermediate [a..] list
-- but would require Bounded and Eq as well, so better go for simplicit
zipWithFrom f a = zipWith f [a..]


-- | A merging of 'unzip' and 'concat'.
--
-- > concatUnzip [("a","AB"),("bc","C")] == ("abc","ABC")
concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . Prelude.unzip

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
--   of a string, but you want to accurately preserve whitespace in the rest of the string.
--
-- > word1 "" == ("", "")
-- > word1 "keyword rest of string" == ("keyword","rest of string")
-- > word1 "  keyword\n  rest of string" == ("keyword","rest of string")
-- > \s -> fst (word1 s) == concat (take 1 $ words s)
-- > \s -> words (snd $ word1 s) == drop 1 (words s)
word1 :: String -> (String, String)
word1 = second trimStart . break isSpace . trimStart

-- | Split the first line off a string.
--
-- > line1 "" == ("", "")
-- > line1 "test" == ("test","")
-- > line1 "test\n" == ("test","")
-- > line1 "test\nrest" == ("test","rest")
-- > line1 "test\nrest\nmore" == ("test","rest\nmore")
line1 :: String -> (String, String)
line1 = second drop1 . break (== '\n')

-- | Escape a string such that it can be inserted into an HTML document or @\"@ attribute
--   without any special interpretation. This requires escaping the @<@, @>@, @&@ and @\"@ characters.
--   Note that it will escape @\"@ and @\'@ even though that is not required in an HTML body (but is not harmful).
--
-- > escapeHTML "this is a test" == "this is a test"
-- > escapeHTML "<b>\"g&t\"</n>" == "&lt;b&gt;&quot;g&amp;t&quot;&lt;/n&gt;"
-- > escapeHTML "t'was another test" == "t&#39;was another test"
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '>' = "&gt;"
        f '<' = "&lt;"
        f '&' = "&amp;"
        f '\"' = "&quot;"
        f '\'' = "&#39;"
        f x = [x]

-- | Invert of 'escapeHTML' (does not do general HTML unescaping)
--
-- > \xs -> unescapeHTML (escapeHTML xs) == xs
unescapeHTML :: String -> String
unescapeHTML ('&':xs)
    | Just xs <- stripPrefix "lt;" xs = '<' : unescapeHTML xs
    | Just xs <- stripPrefix "gt;" xs = '>' : unescapeHTML xs
    | Just xs <- stripPrefix "amp;" xs = '&' : unescapeHTML xs
    | Just xs <- stripPrefix "quot;" xs = '\"' : unescapeHTML xs
    | Just xs <- stripPrefix "#39;" xs = '\'' : unescapeHTML xs
unescapeHTML (x:xs) = x : unescapeHTML xs
unescapeHTML [] = []


-- | Escape a string so it can form part of a JSON literal.
--   This requires escaping the special whitespace and control characters. Additionally,
--   Note that it does /not/ add quote characters around the string.
--
-- > escapeJSON "this is a test" == "this is a test"
-- > escapeJSON "\ttab\nnewline\\" == "\\ttab\\nnewline\\\\"
-- > escapeJSON "\ESC[0mHello" == "\\u001b[0mHello"
escapeJSON :: String -> String
escapeJSON x = concatMap f x
    where f '\"' = "\\\""
          f '\\' = "\\\\"
          -- the spaces are technically optional, but we include them so the JSON is readable
          f '\b' = "\\b"
          f '\f' = "\\f"
          f '\n' = "\\n"
          f '\r' = "\\r"
          f '\t' = "\\t"
          f x | isControl x = "\\u" ++ takeEnd 4 ("0000" ++ showHex (ord x) "")
          f x = [x]

-- | General JSON unescaping, inversion of 'escapeJSON' and all other JSON escapes.
--
-- > \xs -> unescapeJSON (escapeJSON xs) == xs
unescapeJSON :: String -> String
unescapeJSON ('\\':x:xs)
    | x == '\"' = '\"' : unescapeJSON xs
    | x == '\\' = '\\' : unescapeJSON xs
    | x == '/' = '/' : unescapeJSON xs
    | x == 'b' = '\b' : unescapeJSON xs
    | x == 'f' = '\f' : unescapeJSON xs
    | x == 'n' = '\n' : unescapeJSON xs
    | x == 'r' = '\r' : unescapeJSON xs
    | x == 't' = '\t' : unescapeJSON xs
    | x == 'u', let (a,b) = splitAt 4 xs, length a == 4, [(i, "")] <- readHex a = chr i : unescapeJSON b
unescapeJSON (x:xs) = x : unescapeJSON xs
unescapeJSON [] = []


-- | A version of 'group' where the equality is done on some extracted value.
--
-- > groupOn abs [1,-1,2] == [[1,-1], [2]]
groupOn :: Eq k => (a -> k) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y


-- | A version of 'groupOn' which pairs each group with its "key" - the
--   extracted value used for equality testing.
--
-- > groupOnKey abs [1,-1,2] == [(1, [1,-1]), (2, [2])]
groupOnKey :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOnKey _ []     = []
groupOnKey f (x:xs) = (fx, x:yes) : groupOnKey f no
    where
        fx = f x
        (yes, no) = span (\y -> fx == f y) xs


-- | /DEPRECATED/ Use 'nubOrdOn', since this function is _O(n^2)_.
--
--   A version of 'nub' where the equality is done on some extracted value.
--   @nubOn f@ is equivalent to @nubBy ((==) `on` f)@, but has the
--   performance advantage of only evaluating @f@ once for each element in the
--   input list.
{-# DEPRECATED nubOn "Use nubOrdOn, since this function is O(n^2)" #-}
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = map snd . nubBy ((==) `on` fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | A version of 'maximum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > maximumOn id [] == undefined
-- > maximumOn length ["test","extra","a"] == "extra"
maximumOn :: (Partial, Ord b) => (a -> b) -> [a] -> a
maximumOn f [] = error "Data.List.Extra.maximumOn: empty list"
maximumOn f (x:xs) = g x (f x) xs
    where
        g v mv [] = v
        g v mv (x:xs) | mx > mv = g x mx xs
                      | otherwise = g v mv xs
            where mx = f x


-- | A version of 'minimum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
--
-- > minimumOn id [] == undefined
-- > minimumOn length ["test","extra","a"] == "a"
minimumOn :: (Partial, Ord b) => (a -> b) -> [a] -> a
minimumOn f [] = error "Data.List.Extra.minimumOn: empty list"
minimumOn f (x:xs) = g x (f x) xs
    where
        g v mv [] = v
        g v mv (x:xs) | mx < mv = g x mx xs
                      | otherwise = g v mv xs
            where mx = f x

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


-- | A strict version of 'sum'.
--   Unlike 'sum' this function is always strict in the `Num` argument,
--   whereas the standard version is only strict if the optimiser kicks in.
--
-- > sum' [1, 2, 3] == 6
sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0

-- | A strict version of 'sum', using a custom valuation function.
--
-- > sumOn' read ["1", "2", "3"] == 6
sumOn' :: (Num b) => (a -> b) -> [a] -> b
sumOn' f = foldl' (\acc x -> acc + f x) 0

-- | A strict version of 'product'.
--
-- > product' [1, 2, 4] == 8
product' :: (Num a) => [a] -> a
product' = foldl' (*) 1

-- | A strict version of 'product', using a custom valuation function.
--
-- > productOn' read ["1", "2", "4"] == 8
productOn' :: (Num b) => (a -> b) -> [a] -> b
productOn' f = foldl' (\acc x -> acc * f x) 1

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


-- | Replace a subsequence everywhere it occurs.
--
-- > replace "el" "_" "Hello Bella" == "H_lo B_la"
-- > replace "el" "e" "Hello"       == "Helo"
-- > replace "" "x" "Hello"         == "xHxexlxlxox"
-- > replace "" "x" ""              == "x"
-- > \xs ys -> replace xs xs ys == ys
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] to xs = go xs
    where go [] = to
          go (x:xs) = to ++ x : go xs
replace from to xs | Just xs <- stripPrefix from xs = to ++ replace from to xs
replace from to (x:xs) = x : replace from to xs
replace from to [] = []


-- | Break, but from the end.
--
-- > breakEnd isLower "youRE" == ("you","RE")
-- > breakEnd isLower "youre" == ("youre","")
-- > breakEnd isLower "YOURE" == ("","YOURE")
-- > \f xs -> breakEnd (not . f) xs == spanEnd f  xs
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd f = swap . both reverse . break f . reverse

-- | Span, but from the end.
--
-- > spanEnd isUpper "youRE" == ("you","RE")
-- > spanEnd (not . isSpace) "x y z" == ("x y ","z")
-- > \f xs -> uncurry (++) (spanEnd f xs) == xs
-- > \f xs -> spanEnd f xs == swap (both reverse (span f (reverse xs)))
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


-- | Equivalent to @dropEnd 1@, but likely to be faster and a single lexeme.
--
-- > dropEnd1 ""         == ""
-- > dropEnd1 "test"     == "tes"
-- > \xs -> dropEnd 1 xs == dropEnd1 xs
dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x:xs) = foldr (\z f y -> y : f z) (const []) xs x


-- | Version on `concatMap` generalised to a `Monoid` rather than just a list.
--
-- > mconcatMap Sum [1,2,3] == Sum 6
-- > \f xs -> mconcatMap f xs == concatMap f xs
mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f


-- | Find the first instance of @needle@ in @haystack@.
-- The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
-- If you want the remainder /without/ the match, use 'stripInfix'.
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
breakOnEnd needle haystack = both reverse $ swap $ breakOn (reverse needle) (reverse haystack)


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
splitOn :: (Partial, Eq a) => [a] -> [a] -> [[a]]
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


-- | Drops the given prefix from a list.
--   It returns the original sequence if the sequence doesn't start with the given prefix.
--
-- > dropPrefix "Mr. " "Mr. Men" == "Men"
-- > dropPrefix "Mr. " "Dr. Men" == "Dr. Men"
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix a b = fromMaybe b $ stripPrefix a b


-- | Drops the given suffix from a list.
--   It returns the original sequence if the sequence doesn't end with the given suffix.
--
-- > dropSuffix "!" "Hello World!"  == "Hello World"
-- > dropSuffix "!" "Hello World!!" == "Hello World!"
-- > dropSuffix "!" "Hello World."  == "Hello World."
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a b = fromMaybe b $ stripSuffix a b

-- | Return the prefix of the second list if its suffix
--   matches the entire first list.
--
-- Examples:
--
-- > stripSuffix "bar" "foobar" == Just "foo"
-- > stripSuffix ""    "baz"    == Just "baz"
-- > stripSuffix "foo" "quux"   == Nothing
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)


-- | Return the the string before and after the search string,
--   or 'Nothing' if the search string is not present.
--
-- Examples:
--
-- > stripInfix "::" "a::b::c" == Just ("a", "b::c")
-- > stripInfix "/" "foobar"   == Nothing
stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix needle haystack | Just rest <- stripPrefix needle haystack = Just ([], rest)
stripInfix needle [] = Nothing
stripInfix needle (x:xs) = first (x:) <$> stripInfix needle xs


-- | Similar to 'stripInfix', but searches from the end of the
-- string.
--
-- > stripInfixEnd "::" "a::b::c" == Just ("a::b", "c")
stripInfixEnd :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfixEnd needle haystack = both reverse . swap <$> stripInfix (reverse needle) (reverse haystack)


-- | Split a list into chunks of a given size. The last chunk may contain
--   fewer than n elements. The chunk size must be positive.
--
-- > chunksOf 3 "my test" == ["my ","tes","t"]
-- > chunksOf 3 "mytest"  == ["myt","est"]
-- > chunksOf 8 ""        == []
-- > chunksOf 0 "test"    == undefined
chunksOf :: Partial => Int -> [a] -> [[a]]
chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
chunksOf i xs = repeatedly (splitAt i) xs


-- | /O(n log n)/. The 'nubSort' function sorts and removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
--
-- > nubSort "this is a test" == " aehist"
-- > \xs -> nubSort xs == nub (sort xs)
nubSort :: Ord a => [a] -> [a]
nubSort = nubSortBy compare

-- | A version of 'nubSort' which operates on a portion of the value.
--
-- > nubSortOn length ["a","test","of","this"] == ["a","of","test"]
nubSortOn :: Ord b => (a -> b) -> [a] -> [a]
nubSortOn f = nubSortBy (compare `on` f)

-- | A version of 'nubSort' with a custom predicate.
--
-- > nubSortBy (compare `on` length) ["a","test","of","this"] == ["a","of","test"]
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp = f . sortBy cmp
    where f (x1:x2:xs) | cmp x1 x2 == EQ = f (x1:xs)
          f (x:xs) = x : f xs
          f [] = []

-- | /O(n log n)/. The 'nubOrd' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- Unlike the standard 'nub' operator, this version requires an 'Ord' instance
-- and consequently runs asymptotically faster.
--
-- > nubOrd "this is a test" == "this ae"
-- > nubOrd (take 4 ("this" ++ undefined)) == "this"
-- > \xs -> nubOrd xs == nub xs
nubOrd :: Ord a => [a] -> [a]
nubOrd = nubOrdBy compare

-- | A version of 'nubOrd' which operates on a portion of the value.
--
-- > nubOrdOn length ["a","test","of","this"] == ["a","test","of"]
nubOrdOn :: Ord b => (a -> b) -> [a] -> [a]
nubOrdOn f = map snd . nubOrdBy (compare `on` fst) . map (f &&& id)

-- | A version of 'nubOrd' with a custom predicate.
--
-- > nubOrdBy (compare `on` length) ["a","test","of","this"] == ["a","test","of"]
nubOrdBy :: (a -> a -> Ordering) -> [a] -> [a]
nubOrdBy cmp xs = f E xs
    where f seen [] = []
          f seen (x:xs) | memberRB cmp x seen = f seen xs
                        | otherwise = x : f (insertRB cmp x seen) xs

---------------------------------------------------------------------
-- OKASAKI RED BLACK TREE
-- Taken from https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs
-- But with the Color = R|B fused into the tree

data RB a = E | T_R (RB a) a (RB a) | T_B (RB a) a (RB a) deriving Show

{- Insertion and membership test as by Okasaki -}
insertRB :: (a -> a -> Ordering) -> a -> RB a -> RB a
insertRB cmp x s = case ins s of
    T_R a z b -> T_B a z b
    x -> x
    where
    ins E = T_R E x E
    ins s@(T_B a y b) = case cmp x y of
        LT -> lbalance (ins a) y b
        GT -> rbalance a y (ins b)
        EQ -> s
    ins s@(T_R a y b) = case cmp x y of
        LT -> T_R (ins a) y b
        GT -> T_R a y (ins b)
        EQ -> s

memberRB :: (a -> a -> Ordering) -> a -> RB a -> Bool
memberRB cmp x E = False
memberRB cmp x (T_R a y b) = case cmp x y of
    LT -> memberRB cmp x a
    GT -> memberRB cmp x b
    EQ -> True
memberRB cmp x (T_B a y b) = case cmp x y of
    LT -> memberRB cmp x a
    GT -> memberRB cmp x b
    EQ -> True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
lbalance, rbalance :: RB a -> a -> RB a -> RB a
lbalance (T_R a x b) y (T_R c z d) = T_R (T_B a x b) y (T_B c z d)
lbalance (T_R (T_R a x b) y c) z d = T_R (T_B a x b) y (T_B c z d)
lbalance (T_R a x (T_R b y c)) z d = T_R (T_B a x b) y (T_B c z d)
lbalance a x b = T_B a x b
rbalance (T_R a x b) y (T_R c z d) = T_R (T_B a x b) y (T_B c z d)
rbalance a x (T_R b y (T_R c z d)) = T_R (T_B a x b) y (T_B c z d)
rbalance a x (T_R (T_R b y c) z d) = T_R (T_B a x b) y (T_B c z d)
rbalance a x b = T_B a x b


-- | Like 'zipWith', but keep going to the longest value. The function
--   argument will always be given at least one 'Just', and while both
--   lists have items, two 'Just' values.
--
-- > zipWithLongest (,) "a" "xyz" == [(Just 'a', Just 'x'), (Nothing, Just 'y'), (Nothing, Just 'z')]
-- > zipWithLongest (,) "a" "x" == [(Just 'a', Just 'x')]
-- > zipWithLongest (,) "" "x" == [(Nothing, Just 'x')]
zipWithLongest :: (Maybe a -> Maybe b -> c) -> [a] -> [b] -> [c]
zipWithLongest f [] [] = []
zipWithLongest f (x:xs) (y:ys) = f (Just x) (Just y) : zipWithLongest f xs ys
zipWithLongest f [] ys = map (f Nothing . Just) ys
zipWithLongest f xs [] = map ((`f` Nothing) . Just) xs

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
    go [] (_:_) = LT
    go (_:_) [] = GT
    go (_:xs) (_:ys) = go xs ys
