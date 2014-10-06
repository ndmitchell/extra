{-# OPTIONS_GHC -w #-}
module Test(main) where
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Either.Extra
import Data.IORef.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Numeric.Extra
import System.Directory.Extra
import System.Environment.Extra
import System.Info.Extra
import System.IO.Extra
import System.Time.Extra
main :: IO ()
main = do
  test $ breakOn "::" "a::b::c" == ("a", "::b::c")
  test $ breakOn "/" "foobar"   == ("foobar", "")
  test $ breakOnEnd "::" "a::b::c" == ("a::b::", "c")
  test $ splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
  test $ splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
  test $ splitOn "x"    "x"                == ["",""]
  test $ split (=='a') "aabbaca" == ["","","bb","c",""]
  test $ split (=='a') ""        == [""]
  test $ stripSuffix "bar" "foobar" == Just "foo"
  test $ stripSuffix ""    "baz"    == Just "baz"
  test $ stripSuffix "foo" "quux"   == Nothing

test True = putChar '.'
test False = error "failed"
