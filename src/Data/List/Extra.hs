{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | This module extends "Data.List" with extra functions of a similar nature.
--   The package also exports the existing "Data.List" functions.
--   Some of the names and semantics were inspired by the
--   <https://hackage.haskell.org/package/text text> package.
module Data.List.Extra (
    module Data.List,

    -- * String operations
    lower,
    upper,
    trim,
    trimStart,
    trimEnd,
    word1,
    line1,
    escapeHTML,
    escapeJSON,
    unescapeHTML,
    unescapeJSON,

    -- * Splitting
    dropEnd,
    takeEnd,
    splitAtEnd,
    breakEnd,
    spanEnd,
    dropWhileEnd',
    takeWhileEnd,
    stripSuffix,
    stripInfix,
    stripInfixEnd,
    dropPrefix,
    dropSuffix,
    wordsBy,
    linesBy,
    breakOn,
    breakOnEnd,
    splitOn,
    split,
    chunksOf,

    -- * Basics
    headDef,
    lastDef,
    notNull,
    list,
    unsnoc,
    cons,
    snoc,
    drop1,
    dropEnd1,
    mconcatMap,

    -- * Enum operations
    enumerate,

    -- * List operations
    groupSort,
    groupSortOn,
    groupSortBy,
    nubOrd,
    nubOrdBy,
    nubOrdOn,
    nubOn,
    groupOn,
    nubSort,
    nubSortBy,
    nubSortOn,
    maximumOn,
    minimumOn,
    sum',
    product',
    sumOn',
    productOn',
    disjoint,
    disjointOrd,
    disjointOrdBy,
    allSame,
    anySame,
    repeatedly,
    firstJust,
    concatUnzip,
    concatUnzip3,
    zipFrom,
    zipWithFrom,
    zipWithLongest,
    replace,
    merge,
    mergeBy,
) where

import Data.Char (chr, isControl, isSpace, ord, toLower, toUpper)
import qualified Data.Foldable.Extra as FX
import Data.Function.Extra (on, on2)
import Data.List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple.Extra (both, first, second, swap, (&&&), (***))
import Numeric (readHex, showHex)
import Partial (Partial)

-- | Apply some operation repeatedly, producing an element of output
--   and the remainder of the list.
--
-- > \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
-- > \xs -> repeatedly word1 (trim xs) == words xs
-- > \xs -> repeatedly line1 xs == lines xs
repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly _ [] = []
repeatedly f as = b : repeatedly f as'
  where
    (b, as') = f as

-- | Are two lists disjoint, with no elements in common.
--
-- > disjoint [1,2,3] [4,5] == True
-- > disjoint [1,2,3] [4,1] == False
disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs

-- | /O((m+n) log m), m <= n/. Are two lists disjoint, with no elements in common.
--
-- @disjointOrd@ is more strict than `disjoint`. For example, @disjointOrd@ cannot
-- terminate if both lists are inifite, while `disjoint` can.
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
    | FX.comparingLength xs ys == LT = go xs ys
    | otherwise = go ys xs
  where
    go zs = not . any (\a -> memberRB cmp a tree)
      where
        tree = foldl' (flip (insertRB cmp)) E zs

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
    f seen (x : xs) = x `elem` seen || f (x : seen) xs
    f _ [] = False

-- | Are all elements the same.
--
-- > allSame [1,1,2] == False
-- > allSame [1,1,1] == True
-- > allSame [1]     == True
-- > allSame []      == True
-- > allSame (1:1:2:undefined) == False
-- > \xs -> allSame xs == (length (nub xs) <= 1)
allSame :: Eq a => [a] -> Bool
allSame = list True (all . (==))

-- | A total 'head' with a default value.
--
-- > headDef 1 []      == 1
-- > headDef 1 [2,3,4] == 2
-- > \x xs -> headDef x xs == fromMaybe x (listToMaybe xs)
headDef :: a -> [a] -> a
headDef d = fromMaybe d . listToMaybe

-- | A total 'last' with a default value.
--
-- > lastDef 1 []      == 1
-- > lastDef 1 [2,3,4] == 4
-- > \x xs -> lastDef x xs == last (x:xs)
lastDef :: a -> [a] -> a
lastDef = foldl (\_ x -> x) -- I know this looks weird, but apparently this is the fastest way to do this: https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html#last
{-# INLINE lastDef #-}

-- | A composition of 'not' and 'null'.
notNull :: [a] -> Bool
notNull = FX.notNull

-- | Non-recursive transform over a list, like 'maybe'.
--
-- > list 1 (\v _ -> v - 2) [5,6,7] == 3
-- > list 1 (\v _ -> v - 2) []      == 1
-- > \nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs
list :: b -> (a -> [a] -> b) -> [a] -> b
list n _ [] = n
list _ c (x : xs) = c x xs

-- | If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
--
-- > unsnoc "test" == Just ("tes",'t')
-- > unsnoc ""     == Nothing
-- > \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = first (x :) <$> unsnoc xs

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
enumerate = [minBound .. maxBound]

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
  where
    f (_ : ys) (_ : zs) = f ys zs
    f ys _ = ys

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
  where
    f (y : ys) (_ : zs) = y : f ys zs
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
  where
    f (y : ys) (_ : zs) = first (y :) $ f ys zs
    f ys _ = ([], ys)

-- | 'zip' against an enumeration. Truncates the output if one list is shorter.
--
-- > \i xs -> zip [i..] xs == zipFrom i xs
-- > zipFrom False [1..3] == [(False,1),(True, 2)]
zipFrom :: Enum a => a -> [b] -> [(a, b)]
zipFrom = zipWithFrom (,)

-- | 'zipFrom' generalised to any combining operation. Truncates the output if one list is shorter.
--
-- > \i xs -> zipWithFrom (,) i xs == zipFrom i xs
zipWithFrom :: Enum a => (a -> b -> c) -> a -> [b] -> [c]
-- would love to deforest the intermediate [a..] list
-- but would require Bounded and Eq as well, so better go for simplicity
zipWithFrom f a = zipWith f [a ..]

-- | A merging of 'unzip' and 'concat'.
--
-- > concatUnzip [("a","AB"),("bc","C")] == ("abc","ABC")
concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . unzip

-- | A merging of 'unzip3' and 'concat'.
--
-- > concatUnzip3 [("a","AB",""),("bc","C","123")] == ("abc","ABC","123")
concatUnzip3 :: [([a], [b], [c])] -> ([a], [b], [c])
concatUnzip3 xs = (concat a, concat b, concat c)
  where
    (a, b, c) = unzip3 xs

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
escapeHTML = concatMap $ \case
    '>' -> "&gt;"
    '<' -> "&lt;"
    '&' -> "&amp;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    x -> [x]

-- | Invert of 'escapeHTML' (does not do general HTML unescaping)
--
-- > \xs -> unescapeHTML (escapeHTML xs) == xs
unescapeHTML :: String -> String
unescapeHTML ('&' : xs)
    | Just ys <- stripPrefix "lt;" xs = '<' : unescapeHTML ys
    | Just ys <- stripPrefix "gt;" xs = '>' : unescapeHTML ys
    | Just ys <- stripPrefix "amp;" xs = '&' : unescapeHTML ys
    | Just ys <- stripPrefix "quot;" xs = '\"' : unescapeHTML ys
    | Just ys <- stripPrefix "#39;" xs = '\'' : unescapeHTML ys
unescapeHTML (x : xs) = x : unescapeHTML xs
unescapeHTML [] = []

-- | Escape a string so it can form part of a JSON literal.
--   This requires escaping the special whitespace and control characters. Additionally,
--   Note that it does /not/ add quote characters around the string.
--
-- > escapeJSON "this is a test" == "this is a test"
-- > escapeJSON "\ttab\nnewline\\" == "\\ttab\\nnewline\\\\"
-- > escapeJSON "\ESC[0mHello" == "\\u001b[0mHello"
escapeJSON :: String -> String
escapeJSON = concatMap $ \case
    '\"' -> "\\\""
    '\\' -> "\\\\"
    -- the spaces are technically optional, but we include them so the JSON is readable
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    x -> if isControl x then "\\u" ++ takeEnd 4 ("0000" ++ showHex (ord x) "") else [x]

-- | General JSON unescaping, inversion of 'escapeJSON' and all other JSON escapes.
--
-- > \xs -> unescapeJSON (escapeJSON xs) == xs
unescapeJSON :: String -> String
unescapeJSON ('\\' : x : xs) = case x of
    '\"' -> '\"' : unescapeJSON xs
    '\\' -> '\\' : unescapeJSON xs
    '/' -> '/' : unescapeJSON xs
    'b' -> '\b' : unescapeJSON xs
    'f' -> '\f' : unescapeJSON xs
    'n' -> '\n' : unescapeJSON xs
    'r' -> '\r' : unescapeJSON xs
    't' -> '\t' : unescapeJSON xs
    'u' ->
        let (a, b) = splitAt 4 xs
         in if length a == 4
                then let [(i, "")] = readHex a in chr i : unescapeJSON b
                else 'u' : unescapeJSON xs
    _ -> unescapeJSON (x : xs)
unescapeJSON (x : xs) = x : unescapeJSON xs
unescapeJSON [] = []

-- | A version of 'group' where the equality is done on some extracted value.
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)

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
maximumOn :: (Partial, Ord b) => (a -> b) -> [a] -> a
maximumOn = FX.maximumOn

-- | A version of 'minimum' where the comparison is done on some extracted value.
--   Raises an error if the list is empty. Only calls the function once per element.
minimumOn :: (Partial, Ord b) => (a -> b) -> [a] -> a
minimumOn = FX.minimumOn

-- | A combination of 'group' and 'sort'.
--
-- > groupSort [(1,'t'),(3,'t'),(2,'e'),(2,'s')] == [(1,"t"),(2,"es"),(3,"t")]
-- > \xs -> map fst (groupSort xs) == sort (nub (map fst xs))
-- > \xs -> concatMap snd (groupSort xs) == map snd (sortOn fst xs)
groupSort :: Ord k => [(k, v)] -> [(k, [v])]
groupSort = map (fst . head &&& map snd) . groupOn fst . sortOn fst

-- | A combination of 'group' and 'sort', using a part of the value to compare on.
--
-- > groupSortOn length ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = map (map snd) . groupOn fst . sortOn fst . map (f &&& id)

-- | A combination of 'group' and 'sort', using a predicate to compare on.
--
-- > groupSortBy (compare `on` length) ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (((== EQ) .) . f) . sortBy f

-- | A strict version of 'sum'.
--   Unlike 'sum' this function is always strict in the `Num` argument,
--   whereas the standard version is only strict if the optimiser kicks in.
sum' :: (Num a) => [a] -> a
sum' = FX.sum'

-- | A strict version of 'sum', using a custom valuation function.
sumOn' :: (Num b) => (a -> b) -> [a] -> b
sumOn' = FX.sumOn'

-- | A strict version of 'product'.
product' :: (Num a) => [a] -> a
product' = FX.product'

-- | A strict version of 'product', using a custom valuation function.
productOn' :: (Num b) => (a -> b) -> [a] -> b
productOn' = FX.productOn'

-- | Merge two lists which are assumed to be ordered.
--
-- > merge "ace" "bd" == "abcde"
-- > \xs ys -> merge (sort xs) (sort ys) == sort (xs ++ ys)
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

-- | Like 'merge', but with a custom ordering function.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ xs [] = xs
mergeBy _ [] ys = ys
mergeBy f (x : xs) (y : ys)
    | f x y /= GT = x : mergeBy f xs (y : ys)
    | otherwise = y : mergeBy f (x : xs) ys

-- | Replace a subsequence everywhere it occurs. The first argument must
--   not be the empty list.
--
-- > replace "el" "_" "Hello Bella" == "H_lo B_la"
-- > replace "el" "e" "Hello"       == "Helo"
-- > replace "" "e" "Hello"         == undefined
-- > \xs ys -> not (null xs) ==> replace xs xs ys == ys
replace :: (Partial, Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ _ = error "Extra.replace, first argument cannot be empty"
replace from to xs | Just ys <- stripPrefix from xs = to ++ replace from to ys
replace from to (x : xs) = x : replace from to xs
replace _ _ [] = []

-- | Break, but from the end.
--
-- > breakEnd isLower "youRE" == ("you","RE")
-- > breakEnd isLower "youre" == ("youre","")
-- > breakEnd isLower "YOURE" == ("","YOURE")
-- > \f xs -> breakEnd (not . f) xs == spanEnd f xs
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
    x : xs -> (x : w) : wordsBy f (drop1 z)
      where
        (w, z) = break f xs

-- | A variant of 'lines' with a custom test. In particular,
--   if there is a trailing separator it will be discarded.
--
-- > linesBy (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123",""]
-- > \s -> linesBy (== '\n') s == lines s
-- > linesBy (== ';') "my;list;here;" == ["my","list","here"]
linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy _ [] = []
linesBy f zs = g $ case break f zs of
    (l, s) -> (l,) $ case s of
        [] -> []
        _ : ys -> linesBy f ys
  where
    g ~(h, t) = h : t -- to fix a space leak, see the GHC defn of lines

-- | Find the first element of a list for which the operation returns 'Just', along
--   with the result of the operation. Like 'find' but useful where the function also
--   computes some expensive information that can be reused. Particularly useful
--   when the function is monadic, see 'firstJustM'.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust = FX.firstJust

-- | Equivalent to @drop 1@, but likely to be faster and a single lexeme.
--
-- > drop1 ""         == ""
-- > drop1 "test"     == "est"
-- > \xs -> drop 1 xs == drop1 xs
drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_ : xs) = xs

-- | Equivalent to @dropEnd 1@, but likely to be faster and a single lexeme.
--
-- > dropEnd1 ""         == ""
-- > dropEnd1 "test"     == "tes"
-- > \xs -> dropEnd 1 xs == dropEnd1 xs
dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x : xs) = foldr (\z f y -> y : f z) (const []) xs x

-- | Version on `concatMap` generalised to a `Monoid` rather than just a list.
--
--   DEPRECATED: Use 'foldMap' instead.
mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap = foldMap
{-# DEPRECATED mconcatMap "use foldMap" #-}

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
breakOn _ [] = ([], [])
breakOn needle (x : xs) = first (x :) $ breakOn needle xs

-- | Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- > breakOnEnd "::" "a::b::c" == ("a::b::", "c")
breakOnEnd :: Eq a => [a] -> [a] -> ([a], [a])
breakOnEnd = ((both reverse . swap) .) . (breakOn `on` reverse)

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
  where
    (a, b) = breakOn needle haystack

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
split _ [] = [[]]
split f (x : xs) | f x = [] : split f xs
split f (x : xs) = let y : ys = split f xs in (x : y) : ys

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
dropPrefix a = fromMaybe <*> stripPrefix a

-- | Drops the given suffix from a list.
--   It returns the original sequence if the sequence doesn't end with the given suffix.
--
-- > dropSuffix "!" "Hello World!"  == "Hello World"
-- > dropSuffix "!" "Hello World!!" == "Hello World!"
-- > dropSuffix "!" "Hello World."  == "Hello World."
dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix a = fromMaybe <*> stripSuffix a

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
stripInfix _ [] = Nothing
stripInfix needle (x : xs) = first (x :) <$> stripInfix needle xs

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
chunksOf i _ | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
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
  where
    f (x1 : x2 : xs) | cmp x1 x2 == EQ = f (x1 : xs)
    f (x : xs) = x : f xs
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
nubOrdBy cmp = f E
  where
    f _ [] = []
    f seen (y : ys)
        | memberRB cmp y seen = f seen ys
        | otherwise = y : f (insertRB cmp y seen) ys

---------------------------------------------------------------------
-- OKASAKI RED BLACK TREE
-- Taken from https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs
-- But with the Color = R|B fused into the tree

data RB a = E | T_R (RB a) a (RB a) | T_B (RB a) a (RB a) deriving (Show)

{- Insertion and membership test as by Okasaki -}
insertRB :: (a -> a -> Ordering) -> a -> RB a -> RB a
insertRB cmp x s = case ins s of
    T_R a z b -> T_B a z b
    y -> y
  where
    ins E = T_R E x E
    ins t@(T_B a y b) = case cmp x y of
        LT -> lbalance (ins a) y b
        GT -> rbalance a y (ins b)
        EQ -> t
    ins t@(T_R a y b) = case cmp x y of
        LT -> T_R (ins a) y b
        GT -> T_R a y (ins b)
        EQ -> t

memberRB :: (a -> a -> Ordering) -> a -> RB a -> Bool
memberRB _ _ E = False
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
zipWithLongest _ [] [] = []
zipWithLongest f (x : xs) (y : ys) = f (Just x) (Just y) : zipWithLongest f xs ys
zipWithLongest f [] ys = map (f Nothing . Just) ys
zipWithLongest f xs [] = map ((`f` Nothing) . Just) xs
