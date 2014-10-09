{-# LANGUAGE ExtendedDefaultRules #-}
module Test(main) where
import TestUtil
import Extra
import Data.List
import Test.QuickCheck
default(Maybe Bool,Int,Double)
main :: IO ()
main = do
  test "Just False &&^ undefined == Just False" $ Just False &&^ undefined == Just False
  test "Just True &&^ Just True == Just True" $ Just True &&^ Just True == Just True
  test "\\xs -> repeatedly (splitAt 3) xs == chunksOf 3 xs" $ \xs -> repeatedly (splitAt 3) xs == chunksOf 3 xs
  test "\\xs -> repeatedly word1 xs       == words xs" $ \xs -> repeatedly word1 xs       == words xs
  test "for [1,2,3] (+1) == [2,3,4]" $ for [1,2,3] (+1) == [2,3,4]
  test "disjoint [1,2,3] [4,5] == True" $ disjoint [1,2,3] [4,5] == True
  test "disjoint [1,2,3] [4,1] == False" $ disjoint [1,2,3] [4,1] == False
  test "anySame [1,1,2] == True" $ anySame [1,1,2] == True
  test "anySame [1,2,3] == False" $ anySame [1,2,3] == False
  test "allSame [1,1,2] == False" $ allSame [1,1,2] == False
  test "allSame [1,1,1] == True" $ allSame [1,1,1] == True
  test "allSame [1]     == True" $ allSame [1]     == True
  test "allSame []      == True" $ allSame []      == True
  test "breakOn \"::\" \"a::b::c\" == (\"a\", \"::b::c\")" $ breakOn "::" "a::b::c" == ("a", "::b::c")
  test "breakOn \"/\" \"foobar\"   == (\"foobar\", \"\")" $ breakOn "/" "foobar"   == ("foobar", "")
  test "\\needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack" $ \needle haystack -> let (prefix,match) = breakOn needle haystack in prefix ++ match == haystack
  test "breakOnEnd \"::\" \"a::b::c\" == (\"a::b::\", \"c\")" $ breakOnEnd "::" "a::b::c" == ("a::b::", "c")
  test "splitOn \"\\r\\n\" \"a\\r\\nb\\r\\nd\\r\\ne\" == [\"a\",\"b\",\"d\",\"e\"]" $ splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
  test "splitOn \"aaa\"  \"aaaXaaaXaaaXaaa\"  == [\"\",\"X\",\"X\",\"X\",\"\"]" $ splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
  test "splitOn \"x\"    \"x\"                == [\"\",\"\"]" $ splitOn "x"    "x"                == ["",""]
  test "splitOn \"x\"    \"\"                 == [\"\"]" $ splitOn "x"    ""                 == [""]
  test "\\s x -> s /= \"\" ==> intercalate s (splitOn s x) == x" $ \s x -> s /= "" ==> intercalate s (splitOn s x) == x
  test "\\c x -> splitOn [c] x                           == split (==c) x" $ \c x -> splitOn [c] x                           == split (==c) x
  test "split (=='a') \"aabbaca\" == [\"\",\"\",\"bb\",\"c\",\"\"]" $ split (=='a') "aabbaca" == ["","","bb","c",""]
  test "split (=='a') \"\"        == [\"\"]" $ split (=='a') ""        == [""]
  test "stripSuffix \"bar\" \"foobar\" == Just \"foo\"" $ stripSuffix "bar" "foobar" == Just "foo"
  test "stripSuffix \"\"    \"baz\"    == Just \"baz\"" $ stripSuffix ""    "baz"    == Just "baz"
  test "stripSuffix \"foo\" \"quux\"   == Nothing" $ stripSuffix "foo" "quux"   == Nothing
  test "chunksOf 3 \"my test\" == [\"my \",\"tes\",\"t\"]" $ chunksOf 3 "my test" == ["my ","tes","t"]
  test "chunksOf 3 \"mytest\"  == [\"myt\",\"est\"]" $ chunksOf 3 "mytest"  == ["myt","est"]
  test "chunksOf 8 \"\"        == []" $ chunksOf 8 ""        == []
  test "chunksOf 0 \"test\"    == error" $ erroneous $ chunksOf 0 "test"   
  putStrLn "Success (32 tests)"
