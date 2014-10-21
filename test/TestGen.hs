{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}
module TestGen(tests) where
import TestUtil
default(Maybe Bool,Int,Double,Maybe (Maybe Bool),Maybe (Maybe Char))
tests :: IO ()
tests = do
    testGen "stringException (\"test\" ++ undefined)            == return \"test<Exception>\"" $ stringException ("test" ++ undefined)            == return "test<Exception>"
    testGen "stringException (\"test\" ++ undefined ++ \"hello\") == return \"test<Exception>\"" $ stringException ("test" ++ undefined ++ "hello") == return "test<Exception>"
    testGen "stringException \"test\"                           == return \"test\"" $ stringException "test"                           == return "test"
    testGen "ignore (print 1)    == print 1" $ ignore (print 1)    == print 1
    testGen "ignore (fail \"die\") == return ()" $ ignore (fail "die") == return ()
    testGen "retry 1 (print \"x\")  == print \"x\"" $ retry 1 (print "x")  == print "x"
    testGen "retry 3 (fail \"die\") == fail \"die\"" $ retry 3 (fail "die") == fail "die"
    testGen "whenJust Nothing  print == return ()" $ whenJust Nothing  print == return ()
    testGen "whenJust (Just 1) print == print 1" $ whenJust (Just 1) print == print 1
    testGen "\\(x :: Maybe ()) -> unit x == x" $ \(x :: Maybe ()) -> unit x == x
    testGen "partitionM (Just . even) [1,2,3] == Just ([2], [1,3])" $ partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
    testGen "partitionM (const Nothing) [1,2,3] == Nothing" $ partitionM (const Nothing) [1,2,3] == Nothing
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
    testGen "findM (Just . isUpper) \"henRY\"            == Just (Just 'R')" $ findM (Just . isUpper) "henRY"            == Just (Just 'R')
    testGen "findM (Just . isUpper) \"henry\"            == Just Nothing" $ findM (Just . isUpper) "henry"            == Just Nothing
    testGen "findM (Just . const True) [\"x\",undefined] == Just (Just \"x\")" $ findM (Just . const True) ["x",undefined] == Just (Just "x")
    testGen "\\x -> fromLeft (Left  x) == x" $ \x -> fromLeft (Left  x) == x
    testGen "\\x -> fromLeft (Right x) == undefined" $ \x -> erroneous $  fromLeft (Right x)
    testGen "\\x -> fromRight (Right x) == x" $ \x -> fromRight (Right x) == x
    testGen "\\x -> fromRight (Left  x) == undefined" $ \x -> erroneous $  fromRight (Left  x)
    testGen "\\x -> fromEither (Left x ) == x" $ \x -> fromEither (Left x ) == x
    testGen "\\x -> fromEither (Right x) == x" $ \x -> fromEither (Right x) == x
    testGen "\\xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs" $ \xs -> repeatedly (splitAt 3) xs  == chunksOf 3 xs
    testGen "\\xs -> repeatedly word1 (trim xs) == words xs" $ \xs -> repeatedly word1 (trim xs) == words xs
    testGen "for [1,2,3] (+1) == [2,3,4]" $ for [1,2,3] (+1) == [2,3,4]
    testGen "disjoint [1,2,3] [4,5] == True" $ disjoint [1,2,3] [4,5] == True
    testGen "disjoint [1,2,3] [4,1] == False" $ disjoint [1,2,3] [4,1] == False
    testGen "anySame [1,1,2] == True" $ anySame [1,1,2] == True
    testGen "anySame [1,2,3] == False" $ anySame [1,2,3] == False
    testGen "allSame [1,1,2] == False" $ allSame [1,1,2] == False
    testGen "allSame [1,1,1] == True" $ allSame [1,1,1] == True
    testGen "allSame [1]     == True" $ allSame [1]     == True
    testGen "allSame []      == True" $ allSame []      == True
    testGen "lower \"This is A TEST\" == \"this is a test\"" $ lower "This is A TEST" == "this is a test"
    testGen "lower \"\" == \"\"" $ lower "" == ""
    testGen "breakEnd isLower \"youRE\" == (\"you\",\"RE\")" $ breakEnd isLower "youRE" == ("you","RE")
    testGen "breakEnd isLower \"youre\" == (\"youre\",\"\")" $ breakEnd isLower "youre" == ("youre","")
    testGen "breakEnd isLower \"YOURE\" == (\"\",\"YOURE\")" $ breakEnd isLower "YOURE" == ("","YOURE")
    testGen "spanEnd isUpper \"youRE\" == (\"you\",\"RE\")" $ spanEnd isUpper "youRE" == ("you","RE")
    testGen "spanEnd (not . isSpace) \"x y z\" == (\"x y \",\"z\")" $ spanEnd (not . isSpace) "x y z" == ("x y ","z")
    testGen "\\f xs-> spanEnd f xs == swap (both reverse (span f (reverse xs)))" $ \f xs-> spanEnd f xs == swap (both reverse (span f (reverse xs)))
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
    testGen "split (=='a') \"aabbaca\" == [\"\",\"\",\"bb\",\"c\",\"\"]" $ split (=='a') "aabbaca" == ["","","bb","c",""]
    testGen "split (=='a') \"\"        == [\"\"]" $ split (=='a') ""        == [""]
    testGen "stripSuffix \"bar\" \"foobar\" == Just \"foo\"" $ stripSuffix "bar" "foobar" == Just "foo"
    testGen "stripSuffix \"\"    \"baz\"    == Just \"baz\"" $ stripSuffix ""    "baz"    == Just "baz"
    testGen "stripSuffix \"foo\" \"quux\"   == Nothing" $ stripSuffix "foo" "quux"   == Nothing
    testGen "chunksOf 3 \"my test\" == [\"my \",\"tes\",\"t\"]" $ chunksOf 3 "my test" == ["my ","tes","t"]
    testGen "chunksOf 3 \"mytest\"  == [\"myt\",\"est\"]" $ chunksOf 3 "mytest"  == ["myt","est"]
    testGen "chunksOf 8 \"\"        == []" $ chunksOf 8 ""        == []
    testGen "chunksOf 0 \"test\"    == undefined" $ erroneous $ chunksOf 0 "test"   
    testGen "showDP 4 pi == \"3.1416\"" $ showDP 4 pi == "3.1416"
    testGen "showDP 0 pi == \"3\"" $ showDP 0 pi == "3"
    testGen "showDP 2 3  == \"3.00\"" $ showDP 2 3  == "3.00"
    testGen "captureOutput (print 1) == return (\"1\\n\",())" $ captureOutput (print 1) == return ("1\n",())
    testGen "fmap (round . fst) (duration $ sleep 1) == return 1" $ fmap (round . fst) (duration $ sleep 1) == return 1
    testGen "\\a b -> a > b ==> subtractTime a b > 0" $ \a b -> a > b ==> subtractTime a b > 0
    testGen "showDuration 3.435   == \"3.44s\"" $ showDuration 3.435   == "3.44s"
    testGen "showDuration 623.8   == \"10m24s\"" $ showDuration 623.8   == "10m24s"
    testGen "showDuration 62003.8 == \"17h13m\"" $ showDuration 62003.8 == "17h13m"
    testGen "showDuration 1e8     == \"27777h47m\"" $ showDuration 1e8     == "27777h47m"
    testGen "do f <- offsetTimeIncrease; xs <- replicateM 10 f; return $ xs == sort xs" $ do f <- offsetTimeIncrease; xs <- replicateM 10 f; return $ xs == sort xs
