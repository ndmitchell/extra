{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -main-is Test.main #-}
module Test(main) where
import TestUtil
import Extra
import Data.List
import Test.QuickCheck
default(Maybe Bool,Int,Double)
main :: IO ()
main = do
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
  putStrLn "Success"
