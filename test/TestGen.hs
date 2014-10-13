{-# LANGUAGE ExtendedDefaultRules #-}
module TestGen(tests) where
import TestUtil
import Extra
import Data.List
import Test.QuickCheck
default(Maybe Bool,Int,Double)
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
    testGen "Just False &&^ undefined == Just False" $ Just False &&^ undefined == Just False
    testGen "Just True &&^ Just True == Just True" $ Just True &&^ Just True == Just True
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
    testGen "chunksOf 0 \"test\"    == error" $ erroneous $ chunksOf 0 "test"   
    testGen "captureOutput (print 1) == return (\"1\\n\",())" $ captureOutput (print 1) == return ("1\n",())
