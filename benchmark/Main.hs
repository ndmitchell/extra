module Main where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.List.Extra

main :: IO ()
main = defaultMain
  [ bgroup "nubOrd"
    [ bench "Int: Many repeats" $ nf nubOrd intMany
    , bench "Int: Few repeats " $ nf nubOrd intFew
    , bench "String: Many repeats" $ nf nubOrd stringMany
    , bench "String: Few repeats" $ nf nubOrd stringFew
    ]
  ]

inputSize :: Int
inputSize = 100000

intMany :: [Int]
intMany = take inputSize (cycle [1..1000])

intFew :: [Int]
intFew = take inputSize (cycle [1..100000])

stringMany :: [String]
stringMany =
  let
    prefix   = replicate 52 (replicate 10 'h')
    alphabet = fmap pure (['a'..'z'] <> ['A' .. 'Z'])
    input  = zipWith (<>) prefix alphabet
  in
    take inputSize (cycle input)

stringFew :: [String]
stringFew =
  let
    common = [ "the" , "at"  , "there", "some"  , "my"
             , "of"  , "be"  , "use"  , "her"   , "than"
             , "and" , "this", "an"   , "would" , "first"
             , "a"   , "have", "each" , "make"  ,  "water"
             , "to"  , "from", "which", "like"  ,  "been"
             , "in"  ,  "or" , "she"  , "him"   ,  "call"
             , "is"  ,  "one", "do"   , "into"  ,  "who"
             , "you" ,  "had", "how"  , "time"  ,  "oil"
             , "that",  "by" , "their", "has"   ,  "its"
             , "it"  , "word", "if"   , "look"  ,  "now"
             , "he"  ,  "but", "will" , "two"   ,  "find"
             , "was" ,  "not", "up"   , "more"  ,  "long"
             , "for" , "what", "other", "write" ,  "down"
             , "on"  ,  "all", "about", "go"    ,  "day"
             , "are" , "were", "out"  , "see"   ,  "did"
             , "as"  ,  "we" , "many" , "number",  "get"
             , "with", "when", "then" , "no"    ,  "come"
             , "his" , "your", "them" , "way"   ,  "made"
             , "they",  "can", "these", "could" ,  "may"
             , "I"   , "said", "so"   , "people",  "part"
             ]
    addTicks :: Int -> [String] -> [String]
    addTicks 0 ls = ls
    addTicks n ls = ls <> fmap (<> "'") ls <> addTicks (n - 1) ls

    input = addTicks 100 common
  in
    take inputSize (cycle input)
