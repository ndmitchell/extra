-- GENERATED CODE - DO NOT MODIFY
-- See Generate.hs for details of how to generate

{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module TestGen(tests) where
import TestUtil
import qualified Data.Ord
import Test.QuickCheck.Instances.Semigroup ()
default(Maybe Bool,Int,Double,Maybe (Maybe Bool),Maybe (Maybe Char))
tests :: IO ()
tests = do
    let x ||| y = do t1 <- onceFork x; t2 <- onceFork y; t1; t2
    testGen "\\(x :: IO Int) -> void (once x) == pure ()" $ \(x :: IO Int) -> void (once x) == pure ()
    testGen "\\(x :: IO Int) -> join (once x) == x" $ \(x :: IO Int) -> join (once x) == x
    testGen "\\(x :: IO Int) -> (do y <- once x; y; y) == x" $ \(x :: IO Int) -> (do y <- once x; y; y) == x
    testGen "\\(x :: IO Int) -> (do y <- once x; y ||| y) == x" $ \(x :: IO Int) -> (do y <- once x; y ||| y) == x
    testGen "\\(x :: IO Int) -> join (onceFork x) == x" $ \(x :: IO Int) -> join (onceFork x) == x
    testGen "\\(x :: IO Int) -> (do a <- onceFork x; a; a) == x" $ \(x :: IO Int) -> (do a <- onceFork x; a; a) == x
    testGen "stringException \"test\"                           == pure \"test\"" $ stringException "test"                           == pure "test"
    testGen "stringException (\"test\" ++ undefined)            == pure \"test<Exception>\"" $ stringException ("test" ++ undefined)            == pure "test<Exception>"
    testGen "stringException (\"test\" ++ undefined ++ \"hello\") == pure \"test<Exception>\"" $ stringException ("test" ++ undefined ++ "hello") == pure "test<Exception>"
    testGen "stringException ['t','e','s','t',undefined]      == pure \"test<Exception>\"" $ stringException ['t','e','s','t',undefined]      == pure "test<Exception>"
    testGen "ignore (print 1)    == print 1" $ ignore (print 1)    == print 1
    testGen "ignore (fail \"die\") == pure ()" $ ignore (fail "die") == pure ()
    testGen "catch (errorIO \"Hello\") (\\(ErrorCall x) -> pure x) == pure \"Hello\"" $ catch (errorIO "Hello") (\(ErrorCall x) -> pure x) == pure "Hello"
    testGen "seq (errorIO \"foo\") (print 1) == print 1" $ seq (errorIO "foo") (print 1) == print 1
    testGen "catch (assertIO True  >> pure 1) (\\(x :: AssertionFailed) -> pure 2) == pure 1" $ catch (assertIO True  >> pure 1) (\(x :: AssertionFailed) -> pure 2) == pure 1
    testGen "seq (assertIO False) (print 1) == print 1" $ seq (assertIO False) (print 1) == print 1
    testGen "retry 1 (print \"x\")  == print \"x\"" $ retry 1 (print "x")  == print "x"
    testGen "retry 3 (fail \"die\") == fail \"die\"" $ retry 3 (fail "die") == fail "die"
    testGen "whenJust Nothing  print == pure ()" $ whenJust Nothing  print == pure ()
    testGen "whenJust (Just 1) print == print 1" $ whenJust (Just 1) print == print 1
    testGen "pureIf @Maybe True  5 == Just 5" $ pureIf @Maybe True  5 == Just 5
    testGen "pureIf @Maybe False 5 == Nothing" $ pureIf @Maybe False 5 == Nothing
    testGen "pureIf @[]    True  5 == [5]" $ pureIf @[]    True  5 == [5]
    testGen "pureIf @[]    False 5 == []" $ pureIf @[]    False 5 == []
    testGen "whenMaybe True  (print 1) == fmap Just (print 1)" $ whenMaybe True  (print 1) == fmap Just (print 1)
    testGen "whenMaybe False (print 1) == pure Nothing" $ whenMaybe False (print 1) == pure Nothing
    testGen "\\(x :: Maybe ()) -> unit x == x" $ \(x :: Maybe ()) -> unit x == x
    testGen "fold1M (\\x y -> Just x) [] == undefined" $ erroneous $ fold1M (\x y -> Just x) []
    testGen "fold1M (\\x y -> Just $ x + y) [1, 2, 3] == Just 6" $ fold1M (\x y -> Just $ x + y) [1, 2, 3] == Just 6
    testGen "partitionM (Just . even) [1,2,3] == Just ([2], [1,3])" $ partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
    testGen "partitionM (const Nothing) [1,2,3] == Nothing" $ partitionM (const Nothing) [1,2,3] == Nothing
    testGen "loop (\\x -> if x < 10 then Left $ x * 2 else Right $ show x) 1 == \"16\"" $ loop (\x -> if x < 10 then Left $ x * 2 else Right $ show x) 1 == "16"
    testGen "Just True  ||^ undefined  == Just True" $ Just True  ||^ undefined  == Just True
    testGen "Just False ||^ Just True  == Just True" $ Just False ||^ Just True  == Just True
    testGen "Just False ||^ Just False == Just False" $ Just False ||^ Just False == Just False
    testGen "Just False &&^ undefined  == Just False" $ Just False &&^ undefined  == Just False
    testGen "Just True  &&^ Just True  == Just True" $ Just True  &&^ Just True  == Just True
    testGen "Just True  &&^ Just False == Just False" $ Just True  &&^ Just False == Just False
    testGen "anyM Just [False,True ,undefined] == Just True" $ anyM Just [False,True ,undefined] == Just True
    testGen "anyM Just [False,False,undefined] == undefined" $ erroneous $ anyM Just [False,False,undefined]
    testGen "\\(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)" $ \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
    testGen "allM Just [True,False,undefined] == Just False" $ allM Just [True,False,undefined] == Just False
    testGen "allM Just [True,True ,undefined] == undefined" $ erroneous $ allM Just [True,True ,undefined]
    testGen "\\(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)" $ \(f :: Int -> Maybe Bool) xs -> anyM f xs == orM (map f xs)
    testGen "orM [Just False,Just True ,undefined] == Just True" $ orM [Just False,Just True ,undefined] == Just True
    testGen "orM [Just False,Just False,undefined] == undefined" $ erroneous $ orM [Just False,Just False,undefined]
    testGen "\\xs -> Just (or xs) == orM (map Just xs)" $ \xs -> Just (or xs) == orM (map Just xs)
    testGen "andM [Just True,Just False,undefined] == Just False" $ andM [Just True,Just False,undefined] == Just False
    testGen "andM [Just True,Just True ,undefined] == undefined" $ erroneous $ andM [Just True,Just True ,undefined]
    testGen "\\xs -> Just (and xs) == andM (map Just xs)" $ \xs -> Just (and xs) == andM (map Just xs)
    testGen "findM (Just . isUpper) \"teST\"             == Just (Just 'S')" $ findM (Just . isUpper) "teST"             == Just (Just 'S')
    testGen "findM (Just . isUpper) \"test\"             == Just Nothing" $ findM (Just . isUpper) "test"             == Just Nothing
    testGen "findM (Just . const True) [\"x\",undefined] == Just (Just \"x\")" $ findM (Just . const True) ["x",undefined] == Just (Just "x")
    testGen "fromLeft 1 (Left 3) == 3" $ fromLeft 1 (Left 3) == 3
    testGen "fromLeft 1 (Right \"foo\") == 1" $ fromLeft 1 (Right "foo") == 1
    testGen "fromRight 1 (Right 3) == 3" $ fromRight 1 (Right 3) == 3
    testGen "fromRight 1 (Left \"foo\") == 1" $ fromRight 1 (Left "foo") == 1
    testGen "\\x -> fromLeft' (Left  x) == x" $ \x -> fromLeft' (Left  x) == x
    testGen "\\x -> fromLeft' (Right x) == undefined" $ \x -> erroneous $ fromLeft' (Right x)
    testGen "\\x -> fromRight' (Right x) == x" $ \x -> fromRight' (Right x) == x
    testGen "\\x -> fromRight' (Left  x) == undefined" $ \x -> erroneous $ fromRight' (Left  x)
    testGen "\\x -> fromEither (Left x ) == x" $ \x -> fromEither (Left x ) == x
    testGen "\\x -> fromEither (Right x) == x" $ \x -> fromEither (Right x) == x
    testGen "\\a b -> maybeToEither a (Just b) == Right b" $ \a b -> maybeToEither a (Just b) == Right b
    testGen "\\a -> maybeToEither a Nothing == Left a" $ \a -> maybeToEither a Nothing == Left a
    testGen "\\x -> eitherToMaybe (Left x) == Nothing" $ \x -> eitherToMaybe (Left x) == Nothing
    testGen "\\x -> eitherToMaybe (Right x) == Just x" $ \x -> eitherToMaybe (Right x) == Just x
    testGen "mapLeft show (Left 1) == Left \"1\"" $ mapLeft show (Left 1) == Left "1"
    testGen "mapLeft show (Right True) == Right True" $ mapLeft show (Right True) == Right True
    testGen "mapRight show (Left 1) == Left 1" $ mapRight show (Left 1) == Left 1
    testGen "mapRight show (Right True) == Right \"True\"" $ mapRight show (Right True) == Right "True"
    testGen "\\xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs" $ \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
    testGen "\\xs -> repeatedly word1 (trim xs) == words xs" $ \xs -> repeatedly word1 (trim xs) == words xs
    testGen "\\xs -> repeatedly line1 xs == lines xs" $ \xs -> repeatedly line1 xs == lines xs
    testGen "disjoint [1,2,3] [4,5] == True" $ disjoint [1,2,3] [4,5] == True
    testGen "disjoint [1,2,3] [4,1] == False" $ disjoint [1,2,3] [4,1] == False
    testGen "disjointOrd [1,2,3] [4,5] == True" $ disjointOrd [1,2,3] [4,5] == True
    testGen "disjointOrd [1,2,3] [4,1] == False" $ disjointOrd [1,2,3] [4,1] == False
    testGen "disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,5] == True" $ disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,5] == True
    testGen "disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,8] == False" $ disjointOrdBy (compare `on` (`mod` 7)) [1,2,3] [4,8] == False
    testGen "anySame [1,1,2] == True" $ anySame [1,1,2] == True
    testGen "anySame [1,2,3] == False" $ anySame [1,2,3] == False
    testGen "anySame (1:2:1:undefined) == True" $ anySame (1:2:1:undefined) == True
    testGen "anySame [] == False" $ anySame [] == False
    testGen "\\xs -> anySame xs == (length (nub xs) < length xs)" $ \xs -> anySame xs == (length (nub xs) < length xs)
    testGen "allSame [1,1,2] == False" $ allSame [1,1,2] == False
    testGen "allSame [1,1,1] == True" $ allSame [1,1,1] == True
    testGen "allSame [1]     == True" $ allSame [1]     == True
    testGen "allSame []      == True" $ allSame []      == True
    testGen "allSame (1:1:2:undefined) == False" $ allSame (1:1:2:undefined) == False
    testGen "\\xs -> allSame xs == (length (nub xs) <= 1)" $ \xs -> allSame xs == (length (nub xs) <= 1)
    testGen "headDef 1 []      == 1" $ headDef 1 []      == 1
    testGen "headDef 1 [2,3,4] == 2" $ headDef 1 [2,3,4] == 2
    testGen "\\x xs -> headDef x xs == fromMaybe x (listToMaybe xs)" $ \x xs -> headDef x xs == fromMaybe x (listToMaybe xs)
    testGen "lastDef 1 []      == 1" $ lastDef 1 []      == 1
    testGen "lastDef 1 [2,3,4] == 4" $ lastDef 1 [2,3,4] == 4
    testGen "\\x xs -> lastDef x xs == last (x:xs)" $ \x xs -> lastDef x xs == last (x:xs)
    testGen "[2,3,4] !? 1    == Just 3" $ [2,3,4] !? 1    == Just 3
    testGen "[2,3,4] !? (-1) == Nothing" $ [2,3,4] !? (-1) == Nothing
    testGen "[]      !? 0    == Nothing" $ []      !? 0    == Nothing
    testGen "notNull []  == False" $ notNull []  == False
    testGen "notNull [1] == True" $ notNull [1] == True
    testGen "\\xs -> notNull xs == not (null xs)" $ \xs -> notNull xs == not (null xs)
    testGen "list 1 (\\v _ -> v - 2) [5,6,7] == 3" $ list 1 (\v _ -> v - 2) [5,6,7] == 3
    testGen "list 1 (\\v _ -> v - 2) []      == 1" $ list 1 (\v _ -> v - 2) []      == 1
    testGen "\\nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs" $ \nil cons xs -> maybe nil (uncurry cons) (uncons xs) == list nil cons xs
    testGen "unsnoc \"test\" == Just (\"tes\",'t')" $ unsnoc "test" == Just ("tes",'t')
    testGen "unsnoc \"\"     == Nothing" $ unsnoc ""     == Nothing
    testGen "\\xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)" $ \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
    testGen "cons 't' \"est\" == \"test\"" $ cons 't' "est" == "test"
    testGen "\\x xs -> uncons (cons x xs) == Just (x,xs)" $ \x xs -> uncons (cons x xs) == Just (x,xs)
    testGen "snoc \"tes\" 't' == \"test\"" $ snoc "tes" 't' == "test"
    testGen "\\xs x -> unsnoc (snoc xs x) == Just (xs,x)" $ \xs x -> unsnoc (snoc xs x) == Just (xs,x)
    testGen "enumerate == [False, True]" $ enumerate == [False, True]
    testGen "takeEnd 3 \"hello\"  == \"llo\"" $ takeEnd 3 "hello"  == "llo"
    testGen "takeEnd 5 \"bye\"    == \"bye\"" $ takeEnd 5 "bye"    == "bye"
    testGen "takeEnd (-1) \"bye\" == \"\"" $ takeEnd (-1) "bye" == ""
    testGen "\\i xs -> takeEnd i xs `isSuffixOf` xs" $ \i xs -> takeEnd i xs `isSuffixOf` xs
    testGen "\\i xs -> length (takeEnd i xs) == min (max 0 i) (length xs)" $ \i xs -> length (takeEnd i xs) == min (max 0 i) (length xs)
    testGen "dropEnd 3 \"hello\"  == \"he\"" $ dropEnd 3 "hello"  == "he"
    testGen "dropEnd 5 \"bye\"    == \"\"" $ dropEnd 5 "bye"    == ""
    testGen "dropEnd (-1) \"bye\" == \"bye\"" $ dropEnd (-1) "bye" == "bye"
    testGen "\\i xs -> dropEnd i xs `isPrefixOf` xs" $ \i xs -> dropEnd i xs `isPrefixOf` xs
    testGen "\\i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)" $ \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
    testGen "\\i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]" $ \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
    testGen "splitAtEnd 3 \"hello\" == (\"he\",\"llo\")" $ splitAtEnd 3 "hello" == ("he","llo")
    testGen "splitAtEnd 3 \"he\"    == (\"\", \"he\")" $ splitAtEnd 3 "he"    == ("", "he")
    testGen "\\i xs -> uncurry (++) (splitAt i xs) == xs" $ \i xs -> uncurry (++) (splitAt i xs) == xs
    testGen "\\i xs -> splitAtEnd i xs == (dropEnd i xs, takeEnd i xs)" $ \i xs -> splitAtEnd i xs == (dropEnd i xs, takeEnd i xs)
    testGen "\\i xs -> zip [i..] xs == zipFrom i xs" $ \i xs -> zip [i..] xs == zipFrom i xs
    testGen "zipFrom False [1..3] == [(False,1),(True, 2)]" $ zipFrom False [1..3] == [(False,1),(True, 2)]
    testGen "\\i xs -> zipWithFrom (,) i xs == zipFrom i xs" $ \i xs -> zipWithFrom (,) i xs == zipFrom i xs
    testGen "concatUnzip [(\"a\",\"AB\"),(\"bc\",\"C\")] == (\"abc\",\"ABC\")" $ concatUnzip [("a","AB"),("bc","C")] == ("abc","ABC")
    testGen "concatUnzip3 [(\"a\",\"AB\",\"\"),(\"bc\",\"C\",\"123\")] == (\"abc\",\"ABC\",\"123\")" $ concatUnzip3 [("a","AB",""),("bc","C","123")] == ("abc","ABC","123")
    testGen "takeWhileEnd even [2,3,4,6] == [4,6]" $ takeWhileEnd even [2,3,4,6] == [4,6]
    testGen "trim      \"  hello   \" == \"hello\"" $ trim      "  hello   " == "hello"
    testGen "trimStart \"  hello   \" == \"hello   \"" $ trimStart "  hello   " == "hello   "
    testGen "trimEnd   \"  hello   \" == \"  hello\"" $ trimEnd   "  hello   " == "  hello"
    testGen "\\s -> trim s == trimEnd (trimStart s)" $ \s -> trim s == trimEnd (trimStart s)
    testGen "lower \"This is A TEST\" == \"this is a test\"" $ lower "This is A TEST" == "this is a test"
    testGen "lower \"\" == \"\"" $ lower "" == ""
    testGen "upper \"This is A TEST\" == \"THIS IS A TEST\"" $ upper "This is A TEST" == "THIS IS A TEST"
    testGen "upper \"\" == \"\"" $ upper "" == ""
    testGen "word1 \"\" == (\"\", \"\")" $ word1 "" == ("", "")
    testGen "word1 \"keyword rest of string\" == (\"keyword\",\"rest of string\")" $ word1 "keyword rest of string" == ("keyword","rest of string")
    testGen "word1 \"  keyword\\n  rest of string\" == (\"keyword\",\"rest of string\")" $ word1 "  keyword\n  rest of string" == ("keyword","rest of string")
    testGen "\\s -> fst (word1 s) == concat (take 1 $ words s)" $ \s -> fst (word1 s) == concat (take 1 $ words s)
    testGen "\\s -> words (snd $ word1 s) == drop 1 (words s)" $ \s -> words (snd $ word1 s) == drop 1 (words s)
    testGen "line1 \"\" == (\"\", \"\")" $ line1 "" == ("", "")
    testGen "line1 \"test\" == (\"test\",\"\")" $ line1 "test" == ("test","")
    testGen "line1 \"test\\n\" == (\"test\",\"\")" $ line1 "test\n" == ("test","")
    testGen "line1 \"test\\nrest\" == (\"test\",\"rest\")" $ line1 "test\nrest" == ("test","rest")
    testGen "line1 \"test\\nrest\\nmore\" == (\"test\",\"rest\\nmore\")" $ line1 "test\nrest\nmore" == ("test","rest\nmore")
    testGen "escapeHTML \"this is a test\" == \"this is a test\"" $ escapeHTML "this is a test" == "this is a test"
    testGen "escapeHTML \"<b>\\\"g&t\\\"</n>\" == \"&lt;b&gt;&quot;g&amp;t&quot;&lt;/n&gt;\"" $ escapeHTML "<b>\"g&t\"</n>" == "&lt;b&gt;&quot;g&amp;t&quot;&lt;/n&gt;"
    testGen "escapeHTML \"t'was another test\" == \"t&#39;was another test\"" $ escapeHTML "t'was another test" == "t&#39;was another test"
    testGen "\\xs -> unescapeHTML (escapeHTML xs) == xs" $ \xs -> unescapeHTML (escapeHTML xs) == xs
    testGen "escapeJSON \"this is a test\" == \"this is a test\"" $ escapeJSON "this is a test" == "this is a test"
    testGen "escapeJSON \"\\ttab\\nnewline\\\\\" == \"\\\\ttab\\\\nnewline\\\\\\\\\"" $ escapeJSON "\ttab\nnewline\\" == "\\ttab\\nnewline\\\\"
    testGen "escapeJSON \"\\ESC[0mHello\" == \"\\\\u001b[0mHello\"" $ escapeJSON "\ESC[0mHello" == "\\u001b[0mHello"
    testGen "\\xs -> unescapeJSON (escapeJSON xs) == xs" $ \xs -> unescapeJSON (escapeJSON xs) == xs
    testGen "groupOn abs [1,-1,2] == [[1,-1], [2]]" $ groupOn abs [1,-1,2] == [[1,-1], [2]]
    testGen "groupOnKey abs [1,-1,2] == [(1, [1,-1]), (2, [2])]" $ groupOnKey abs [1,-1,2] == [(1, [1,-1]), (2, [2])]
    testGen "maximumOn id [] == undefined" $ erroneous $ maximumOn id []
    testGen "maximumOn length [\"test\",\"extra\",\"a\"] == \"extra\"" $ maximumOn length ["test","extra","a"] == "extra"
    testGen "minimumOn id [] == undefined" $ erroneous $ minimumOn id []
    testGen "minimumOn length [\"test\",\"extra\",\"a\"] == \"a\"" $ minimumOn length ["test","extra","a"] == "a"
    testGen "groupSort [(1,'t'),(3,'t'),(2,'e'),(2,'s')] == [(1,\"t\"),(2,\"es\"),(3,\"t\")]" $ groupSort [(1,'t'),(3,'t'),(2,'e'),(2,'s')] == [(1,"t"),(2,"es"),(3,"t")]
    testGen "\\xs -> map fst (groupSort xs) == sort (nub (map fst xs))" $ \xs -> map fst (groupSort xs) == sort (nub (map fst xs))
    testGen "\\xs -> concatMap snd (groupSort xs) == map snd (sortOn fst xs)" $ \xs -> concatMap snd (groupSort xs) == map snd (sortOn fst xs)
    testGen "groupSortOn length [\"test\",\"of\",\"sized\",\"item\"] == [[\"of\"],[\"test\",\"item\"],[\"sized\"]]" $ groupSortOn length ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
    testGen "groupSortBy (compare `on` length) [\"test\",\"of\",\"sized\",\"item\"] == [[\"of\"],[\"test\",\"item\"],[\"sized\"]]" $ groupSortBy (compare `on` length) ["test","of","sized","item"] == [["of"],["test","item"],["sized"]]
    testGen "sum' [1, 2, 3] == 6" $ sum' [1, 2, 3] == 6
    testGen "sumOn' read [\"1\", \"2\", \"3\"] == 6" $ sumOn' read ["1", "2", "3"] == 6
    testGen "product' [1, 2, 4] == 8" $ product' [1, 2, 4] == 8
    testGen "productOn' read [\"1\", \"2\", \"4\"] == 8" $ productOn' read ["1", "2", "4"] == 8
    testGen "merge \"ace\" \"bd\" == \"abcde\"" $ merge "ace" "bd" == "abcde"
    testGen "\\xs ys -> merge (sort xs) (sort ys) == sort (xs ++ ys)" $ \xs ys -> merge (sort xs) (sort ys) == sort (xs ++ ys)
    testGen "replace \"el\" \"_\" \"Hello Bella\" == \"H_lo B_la\"" $ replace "el" "_" "Hello Bella" == "H_lo B_la"
    testGen "replace \"el\" \"e\" \"Hello\"       == \"Helo\"" $ replace "el" "e" "Hello"       == "Helo"
    testGen "replace \"\" \"e\" \"Hello\"         == undefined" $ erroneous $ replace "" "e" "Hello"
    testGen "\\xs ys -> not (null xs) ==> replace xs xs ys == ys" $ \xs ys -> not (null xs) ==> replace xs xs ys == ys
    testGen "breakEnd isLower \"youRE\" == (\"you\",\"RE\")" $ breakEnd isLower "youRE" == ("you","RE")
    testGen "breakEnd isLower \"youre\" == (\"youre\",\"\")" $ breakEnd isLower "youre" == ("youre","")
    testGen "breakEnd isLower \"YOURE\" == (\"\",\"YOURE\")" $ breakEnd isLower "YOURE" == ("","YOURE")
    testGen "\\f xs -> breakEnd (not . f) xs == spanEnd f  xs" $ \f xs -> breakEnd (not . f) xs == spanEnd f  xs
    testGen "spanEnd isUpper \"youRE\" == (\"you\",\"RE\")" $ spanEnd isUpper "youRE" == ("you","RE")
    testGen "spanEnd (not . isSpace) \"x y z\" == (\"x y \",\"z\")" $ spanEnd (not . isSpace) "x y z" == ("x y ","z")
    testGen "\\f xs -> uncurry (++) (spanEnd f xs) == xs" $ \f xs -> uncurry (++) (spanEnd f xs) == xs
    testGen "\\f xs -> spanEnd f xs == swap (both reverse (span f (reverse xs)))" $ \f xs -> spanEnd f xs == swap (both reverse (span f (reverse xs)))
    testGen "wordsBy (== ':') \"::xyz:abc::123::\" == [\"xyz\",\"abc\",\"123\"]" $ wordsBy (== ':') "::xyz:abc::123::" == ["xyz","abc","123"]
    testGen "\\s -> wordsBy isSpace s == words s" $ \s -> wordsBy isSpace s == words s
    testGen "linesBy (== ':') \"::xyz:abc::123::\" == [\"\",\"\",\"xyz\",\"abc\",\"\",\"123\",\"\"]" $ linesBy (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123",""]
    testGen "\\s -> linesBy (== '\\n') s == lines s" $ \s -> linesBy (== '\n') s == lines s
    testGen "linesBy (== ';') \"my;list;here;\" == [\"my\",\"list\",\"here\"]" $ linesBy (== ';') "my;list;here;" == ["my","list","here"]
    testGen "firstJust id [Nothing,Just 3]  == Just 3" $ firstJust id [Nothing,Just 3]  == Just 3
    testGen "firstJust id [Nothing,Nothing] == Nothing" $ firstJust id [Nothing,Nothing] == Nothing
    testGen "drop1 \"\"         == \"\"" $ drop1 ""         == ""
    testGen "drop1 \"test\"     == \"est\"" $ drop1 "test"     == "est"
    testGen "\\xs -> drop 1 xs == drop1 xs" $ \xs -> drop 1 xs == drop1 xs
    testGen "dropEnd1 \"\"         == \"\"" $ dropEnd1 ""         == ""
    testGen "dropEnd1 \"test\"     == \"tes\"" $ dropEnd1 "test"     == "tes"
    testGen "\\xs -> dropEnd 1 xs == dropEnd1 xs" $ \xs -> dropEnd 1 xs == dropEnd1 xs
    testGen "mconcatMap Sum [1,2,3] == Sum 6" $ mconcatMap Sum [1,2,3] == Sum 6
    testGen "\\f xs -> mconcatMap f xs == concatMap f xs" $ \f xs -> mconcatMap f xs == concatMap f xs
    testGen "breakOn \"::\" \"a::b::c\" == (\"a\", \"::b::c\")" $ breakOn "::" "a::b::c" == ("a", "::b::c")
    testGen "breakOn \"/\" \"foobar\"   == (\"foobar\", \"\")" $ breakOn "/" "foobar"   == ("foobar", "")
    testGen "\\needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack" $ \needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack
    testGen "breakOnEnd \"::\" \"a::b::c\" == (\"a::b::\", \"c\")" $ breakOnEnd "::" "a::b::c" == ("a::b::", "c")
    testGen "splitOn \"\\r\\n\" \"a\\r\\nb\\r\\nd\\r\\ne\" == [\"a\",\"b\",\"d\",\"e\"]" $ splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
    testGen "splitOn \"aaa\"  \"aaaXaaaXaaaXaaa\"  == [\"\",\"X\",\"X\",\"X\",\"\"]" $ splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
    testGen "splitOn \"x\"    \"x\"                == [\"\",\"\"]" $ splitOn "x"    "x"                == ["",""]
    testGen "splitOn \"x\"    \"\"                 == [\"\"]" $ splitOn "x"    ""                 == [""]
    testGen "\\s x -> s /= \"\" ==> intercalate s (splitOn s x) == x" $ \s x -> s /= "" ==> intercalate s (splitOn s x) == x
    testGen "\\c x -> splitOn [c] x                           == split (==c) x" $ \c x -> splitOn [c] x                           == split (==c) x
    testGen "split (== 'a') \"aabbaca\" == [\"\",\"\",\"bb\",\"c\",\"\"]" $ split (== 'a') "aabbaca" == ["","","bb","c",""]
    testGen "split (== 'a') \"\"        == [\"\"]" $ split (== 'a') ""        == [""]
    testGen "split (== ':') \"::xyz:abc::123::\" == [\"\",\"\",\"xyz\",\"abc\",\"\",\"123\",\"\",\"\"]" $ split (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123","",""]
    testGen "split (== ',') \"my,list,here\" == [\"my\",\"list\",\"here\"]" $ split (== ',') "my,list,here" == ["my","list","here"]
    testGen "dropWhileEnd  isSpace \"ab cde  \" == \"ab cde\"" $ dropWhileEnd  isSpace "ab cde  " == "ab cde"
    testGen "dropWhileEnd' isSpace \"ab cde  \" == \"ab cde\"" $ dropWhileEnd' isSpace "ab cde  " == "ab cde"
    testGen "last (dropWhileEnd  even [undefined,3]) == undefined" $ erroneous $ last (dropWhileEnd  even [undefined,3])
    testGen "last (dropWhileEnd' even [undefined,3]) == 3" $ last (dropWhileEnd' even [undefined,3]) == 3
    testGen "head (dropWhileEnd  even (3:undefined)) == 3" $ head (dropWhileEnd  even (3:undefined)) == 3
    testGen "head (dropWhileEnd' even (3:undefined)) == undefined" $ erroneous $ head (dropWhileEnd' even (3:undefined))
    testGen "dropPrefix \"Mr. \" \"Mr. Men\" == \"Men\"" $ dropPrefix "Mr. " "Mr. Men" == "Men"
    testGen "dropPrefix \"Mr. \" \"Dr. Men\" == \"Dr. Men\"" $ dropPrefix "Mr. " "Dr. Men" == "Dr. Men"
    testGen "dropSuffix \"!\" \"Hello World!\"  == \"Hello World\"" $ dropSuffix "!" "Hello World!"  == "Hello World"
    testGen "dropSuffix \"!\" \"Hello World!!\" == \"Hello World!\"" $ dropSuffix "!" "Hello World!!" == "Hello World!"
    testGen "dropSuffix \"!\" \"Hello World.\"  == \"Hello World.\"" $ dropSuffix "!" "Hello World."  == "Hello World."
    testGen "stripSuffix \"bar\" \"foobar\" == Just \"foo\"" $ stripSuffix "bar" "foobar" == Just "foo"
    testGen "stripSuffix \"\"    \"baz\"    == Just \"baz\"" $ stripSuffix ""    "baz"    == Just "baz"
    testGen "stripSuffix \"foo\" \"quux\"   == Nothing" $ stripSuffix "foo" "quux"   == Nothing
    testGen "stripInfix \"::\" \"a::b::c\" == Just (\"a\", \"b::c\")" $ stripInfix "::" "a::b::c" == Just ("a", "b::c")
    testGen "stripInfix \"/\" \"foobar\"   == Nothing" $ stripInfix "/" "foobar"   == Nothing
    testGen "stripInfixEnd \"::\" \"a::b::c\" == Just (\"a::b\", \"c\")" $ stripInfixEnd "::" "a::b::c" == Just ("a::b", "c")
    testGen "chunksOf 3 \"my test\" == [\"my \",\"tes\",\"t\"]" $ chunksOf 3 "my test" == ["my ","tes","t"]
    testGen "chunksOf 3 \"mytest\"  == [\"myt\",\"est\"]" $ chunksOf 3 "mytest"  == ["myt","est"]
    testGen "chunksOf 8 \"\"        == []" $ chunksOf 8 ""        == []
    testGen "chunksOf 0 \"test\"    == undefined" $ erroneous $ chunksOf 0 "test"
    testGen "nubSort \"this is a test\" == \" aehist\"" $ nubSort "this is a test" == " aehist"
    testGen "\\xs -> nubSort xs == nub (sort xs)" $ \xs -> nubSort xs == nub (sort xs)
    testGen "nubSortOn length [\"a\",\"test\",\"of\",\"this\"] == [\"a\",\"of\",\"test\"]" $ nubSortOn length ["a","test","of","this"] == ["a","of","test"]
    testGen "nubSortBy (compare `on` length) [\"a\",\"test\",\"of\",\"this\"] == [\"a\",\"of\",\"test\"]" $ nubSortBy (compare `on` length) ["a","test","of","this"] == ["a","of","test"]
    testGen "nubOrd \"this is a test\" == \"this ae\"" $ nubOrd "this is a test" == "this ae"
    testGen "nubOrd (take 4 (\"this\" ++ undefined)) == \"this\"" $ nubOrd (take 4 ("this" ++ undefined)) == "this"
    testGen "\\xs -> nubOrd xs == nub xs" $ \xs -> nubOrd xs == nub xs
    testGen "nubOrdOn length [\"a\",\"test\",\"of\",\"this\"] == [\"a\",\"test\",\"of\"]" $ nubOrdOn length ["a","test","of","this"] == ["a","test","of"]
    testGen "nubOrdBy (compare `on` length) [\"a\",\"test\",\"of\",\"this\"] == [\"a\",\"test\",\"of\"]" $ nubOrdBy (compare `on` length) ["a","test","of","this"] == ["a","test","of"]
    testGen "zipWithLongest (,) \"a\" \"xyz\" == [(Just 'a', Just 'x'), (Nothing, Just 'y'), (Nothing, Just 'z')]" $ zipWithLongest (,) "a" "xyz" == [(Just 'a', Just 'x'), (Nothing, Just 'y'), (Nothing, Just 'z')]
    testGen "zipWithLongest (,) \"a\" \"x\" == [(Just 'a', Just 'x')]" $ zipWithLongest (,) "a" "x" == [(Just 'a', Just 'x')]
    testGen "zipWithLongest (,) \"\" \"x\" == [(Nothing, Just 'x')]" $ zipWithLongest (,) "" "x" == [(Nothing, Just 'x')]
    testGen "compareLength [1,2,3] 1 == GT" $ compareLength [1,2,3] 1 == GT
    testGen "compareLength [1,2] 2 == EQ" $ compareLength [1,2] 2 == EQ
    testGen "\\(xs :: [Int]) n -> compareLength xs n == compare (length xs) n" $ \(xs :: [Int]) n -> compareLength xs n == compare (length xs) n
    testGen "compareLength (1:2:3:undefined) 2 == GT" $ compareLength (1:2:3:undefined) 2 == GT
    testGen "comparingLength [1,2,3] [False] == GT" $ comparingLength [1,2,3] [False] == GT
    testGen "comparingLength [1,2] \"ab\" == EQ" $ comparingLength [1,2] "ab" == EQ
    testGen "\\(xs :: [Int]) (ys :: [Int]) -> comparingLength xs ys == Data.Ord.comparing length xs ys" $ \(xs :: [Int]) (ys :: [Int]) -> comparingLength xs ys == Data.Ord.comparing length xs ys
    testGen "comparingLength [1,2] (1:2:3:undefined) == LT" $ comparingLength [1,2] (1:2:3:undefined) == LT
    testGen "comparingLength (1:2:3:undefined) [1,2] == GT" $ comparingLength (1:2:3:undefined) [1,2] == GT
    testGen "first succ (1,\"test\") == (2,\"test\")" $ first succ (1,"test") == (2,"test")
    testGen "second reverse (1,\"test\") == (1,\"tset\")" $ second reverse (1,"test") == (1,"tset")
    testGen "firstM (\\x -> [x-1, x+1]) (1,\"test\") == [(0,\"test\"),(2,\"test\")]" $ firstM (\x -> [x-1, x+1]) (1,"test") == [(0,"test"),(2,"test")]
    testGen "secondM (\\x -> [reverse x, x]) (1,\"test\") == [(1,\"tset\"),(1,\"test\")]" $ secondM (\x -> [reverse x, x]) (1,"test") == [(1,"tset"),(1,"test")]
    testGen "(succ *** reverse) (1,\"test\") == (2,\"tset\")" $ (succ *** reverse) (1,"test") == (2,"tset")
    testGen "(succ &&& pred) 1 == (2,0)" $ (succ &&& pred) 1 == (2,0)
    testGen "dupe 12 == (12, 12)" $ dupe 12 == (12, 12)
    testGen "both succ (1,2) == (2,3)" $ both succ (1,2) == (2,3)
    testGen "first3 succ (1,1,1) == (2,1,1)" $ first3 succ (1,1,1) == (2,1,1)
    testGen "second3 succ (1,1,1) == (1,2,1)" $ second3 succ (1,1,1) == (1,2,1)
    testGen "third3 succ (1,1,1) == (1,1,2)" $ third3 succ (1,1,1) == (1,1,2)
    testGen "\\x -> readVersion (showVersion x) == x" $ \x -> readVersion (showVersion x) == x
    testGen "readVersion \"hello\" == undefined" $ erroneous $ readVersion "hello"
    testGen "showDP 4 pi == \"3.1416\"" $ showDP 4 pi == "3.1416"
    testGen "showDP 0 pi == \"3\"" $ showDP 0 pi == "3"
    testGen "showDP 2 3  == \"3.00\"" $ showDP 2 3  == "3.00"
    testGen "withTempDir $ \\dir -> do writeFile (dir </> \"foo.txt\") \"\"; withCurrentDirectory dir $ doesFileExist \"foo.txt\"" $ withTempDir $ \dir -> do writeFile (dir </> "foo.txt") ""; withCurrentDirectory dir $ doesFileExist "foo.txt"
    testGen "withTempDir $ \\dir -> do writeFile (dir </> \"test.txt\") \"\"; (== [dir </> \"test.txt\"]) <$> listContents dir" $ withTempDir $ \dir -> do writeFile (dir </> "test.txt") ""; (== [dir </> "test.txt"]) <$> listContents dir
    let touch = mapM_ $ \x -> createDirectoryIfMissing True (takeDirectory x) >> writeFile x ""
    let listTest op as bs = withTempDir $ \dir -> do touch $ map (dir </>) as; res <- op dir; pure $ map (drop (length dir + 1)) res == bs
    testGen "listTest listContents [\"bar.txt\",\"foo/baz.txt\",\"zoo\"] [\"bar.txt\",\"foo\",\"zoo\"]" $ listTest listContents ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","foo","zoo"]
    testGen "listTest listDirectories [\"bar.txt\",\"foo/baz.txt\",\"zoo\"] [\"foo\"]" $ listTest listDirectories ["bar.txt","foo/baz.txt","zoo"] ["foo"]
    testGen "listTest listFiles [\"bar.txt\",\"foo/baz.txt\",\"zoo\"] [\"bar.txt\",\"zoo\"]" $ listTest listFiles ["bar.txt","foo/baz.txt","zoo"] ["bar.txt","zoo"]
    testGen "listTest listFilesRecursive [\"bar.txt\",\"zoo\",\"foo\" </> \"baz.txt\"] [\"bar.txt\",\"zoo\",\"foo\" </> \"baz.txt\"]" $ listTest listFilesRecursive ["bar.txt","zoo","foo" </> "baz.txt"] ["bar.txt","zoo","foo" </> "baz.txt"]
    testGen "listTest (listFilesInside $ pure . not . isPrefixOf \".\" . takeFileName)    [\"bar.txt\",\"foo\" </> \"baz.txt\",\".foo\" </> \"baz2.txt\", \"zoo\"] [\"bar.txt\",\"zoo\",\"foo\" </> \"baz.txt\"]" $ listTest (listFilesInside $ pure . not . isPrefixOf "." . takeFileName)    ["bar.txt","foo" </> "baz.txt",".foo" </> "baz2.txt", "zoo"] ["bar.txt","zoo","foo" </> "baz.txt"]
    testGen "listTest (listFilesInside $ const $ pure False) [\"bar.txt\"] []" $ listTest (listFilesInside $ const $ pure False) ["bar.txt"] []
    testGen "isWindows == (os == \"mingw32\")" $ isWindows == (os == "mingw32")
    testGen "\\(filter isHexDigit -> s) -> fmap (== s) $ withTempFile $ \\file -> do writeFile file s; readFile' file" $ \(filter isHexDigit -> s) -> fmap (== s) $ withTempFile $ \file -> do writeFile file s; readFile' file
    testGen "\\s -> withTempFile $ \\file -> do writeFileUTF8 file s; fmap (== s) $ readFileUTF8' file" $ \s -> withTempFile $ \file -> do writeFileUTF8 file s; fmap (== s) $ readFileUTF8' file
    testGen "\\(ASCIIString s) -> withTempFile $ \\file -> do writeFileBinary file s; fmap (== s) $ readFileBinary' file" $ \(ASCIIString s) -> withTempFile $ \file -> do writeFileBinary file s; fmap (== s) $ readFileBinary' file
    testGen "captureOutput (print 1) == pure (\"1\\n\",())" $ captureOutput (print 1) == pure ("1\n",())
    testGen "withTempFile doesFileExist == pure True" $ withTempFile doesFileExist == pure True
    testGen "(doesFileExist =<< withTempFile pure) == pure False" $ (doesFileExist =<< withTempFile pure) == pure False
    testGen "withTempFile readFile' == pure \"\"" $ withTempFile readFile' == pure ""
    testGen "withTempDir doesDirectoryExist == pure True" $ withTempDir doesDirectoryExist == pure True
    testGen "(doesDirectoryExist =<< withTempDir pure) == pure False" $ (doesDirectoryExist =<< withTempDir pure) == pure False
    testGen "withTempDir listFiles == pure []" $ withTempDir listFiles == pure []
    testGen "fileEq \"does_not_exist1\" \"does_not_exist2\" == undefined" $ erroneousIO $ fileEq "does_not_exist1" "does_not_exist2"
    testGen "fileEq \"does_not_exist\" \"does_not_exist\" == undefined" $ erroneousIO $ fileEq "does_not_exist" "does_not_exist"
    testGen "withTempFile $ \\f1 -> fileEq \"does_not_exist\" f1 == undefined" $ erroneousIO $ withTempFile $ \f1 -> fileEq "does_not_exist" f1
    testGen "withTempFile $ \\f1 -> withTempFile $ \\f2 -> fileEq f1 f2" $ withTempFile $ \f1 -> withTempFile $ \f2 -> fileEq f1 f2
    testGen "withTempFile $ \\f1 -> withTempFile $ \\f2 -> writeFile f1 \"a\" >> writeFile f2 \"a\" >> fileEq f1 f2" $ withTempFile $ \f1 -> withTempFile $ \f2 -> writeFile f1 "a" >> writeFile f2 "a" >> fileEq f1 f2
    testGen "withTempFile $ \\f1 -> withTempFile $ \\f2 -> writeFile f1 \"a\" >> writeFile f2 \"b\" >> notM (fileEq f1 f2)" $ withTempFile $ \f1 -> withTempFile $ \f2 -> writeFile f1 "a" >> writeFile f2 "b" >> notM (fileEq f1 f2)
    testGen "fmap (round . fst) (duration $ sleep 1) == pure 1" $ fmap (round . fst) (duration $ sleep 1) == pure 1
    testGen "timeout (-3) (print 1) == pure Nothing" $ timeout (-3) (print 1) == pure Nothing
    testGen "timeout 0.1  (print 1) == fmap Just (print 1)" $ timeout 0.1  (print 1) == fmap Just (print 1)
    testGen "do (t, _) <- duration $ timeout 0.1 $ sleep 1000; print t; pure $ t < 1" $ do (t, _) <- duration $ timeout 0.1 $ sleep 1000; print t; pure $ t < 1
    testGen "timeout 0.1  (sleep 2 >> print 1) == pure Nothing" $ timeout 0.1  (sleep 2 >> print 1) == pure Nothing
    testGen "showDuration 3.435   == \"3.44s\"" $ showDuration 3.435   == "3.44s"
    testGen "showDuration 623.8   == \"10m24s\"" $ showDuration 623.8   == "10m24s"
    testGen "showDuration 62003.8 == \"17h13m\"" $ showDuration 62003.8 == "17h13m"
    testGen "showDuration 1e8     == \"27777h47m\"" $ showDuration 1e8     == "27777h47m"
    testGen "do f <- offsetTime; xs <- replicateM 10 f; pure $ xs == sort xs" $ do f <- offsetTime; xs <- replicateM 10 f; pure $ xs == sort xs
    testGen "do (a,_) <- duration $ sleep 1; pure $ a >= 1 && a <= 1.5" $ do (a,_) <- duration $ sleep 1; pure $ a >= 1 && a <= 1.5
