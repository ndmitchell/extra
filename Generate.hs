-- This module generates the files src/Extra.hs and test/TestGen.hs.
-- Either call "runghc Generate" or start "ghci" and use ":generate".

module Generate(main) where

import Data.List.Extra
import System.IO.Extra
import Control.Exception
import Control.Monad.Extra
import System.FilePath
import System.Directory
import Data.Char
import Data.Maybe
import Data.Functor
import Prelude


main :: IO ()
main = do
    src <- readFile "extra.cabal"
    let mods = filter (isSuffixOf ".Extra") $ map trim $ lines src
    ifaces <- forM (mods \\ exclude) $ \mod -> do
        src <- readFile $ joinPath ("src" : split (== '.') mod) <.> "hs"
        let funcs = filter validIdentifier $ takeWhile (/= "where") $
                    words $ replace "," " " $ drop1 $ dropWhile (/= '(') $
                    unlines $ filter (\x -> not $ any (`isPrefixOf` trim x) ["--","#"]) $ lines src
        let tests = if mod `elem` excludeTests then [] else mapMaybe (stripPrefix "-- > ") $ lines src
        pure (mod, funcs, tests)
    writeFileBinaryChanged "src/Extra.hs" $ unlines $
        ["-- GENERATED CODE - DO NOT MODIFY"
        ,"-- See Generate.hs for details of how to generate"
        ,""
        ,"-- | This module documents all the functions available in this package."
        ,"--"
        ,"--   Most users should import the specific modules (e.g. @\"Data.List.Extra\"@), which"
        ,"--   also reexport their non-@Extra@ modules (e.g. @\"Data.List\"@)."
        ,"module Extra {-# DEPRECATED \"This module is provided as documentation of all new functions, you should import the more specific modules directly.\" #-} ("] ++
        concat [ ["    -- * " ++ mod
                 ,"    -- | Extra functions available in @" ++ show mod ++ "@."
                 ,"    " ++ unwords (map (++",") $ filter (notHidden mod) funs)]
               | (mod,funs@(_:_),_) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ addHiding mod | (mod,_:_,_) <- ifaces]
    writeFileBinaryChanged "test/TestGen.hs" $ unlines $
        ["-- GENERATED CODE - DO NOT MODIFY"
        ,"-- See Generate.hs for details of how to generate"
        ,""
        ,"{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables, TypeApplications, ViewPatterns #-}"
        ,"module TestGen(tests) where"
        ,"import TestUtil"
        ,"import qualified Data.Ord"
        ,"import Test.QuickCheck.Instances.Semigroup ()"
        ,"default(Maybe Bool,Int,Double,Maybe (Maybe Bool),Maybe (Maybe Char))"
        ,"tests :: IO ()"
        ,"tests = do"] ++
        ["    " ++ if "let " `isPrefixOf` t then t else "testGen " ++ show t ++ " $ " ++ tweakTest t | (_,_,ts) <- ifaces, t <- rejoin ts]

rejoin :: [String] -> [String]
rejoin (x1:x2:xs) | " " `isPrefixOf` x2 = rejoin $ (x1 ++ x2) : xs
rejoin (x:xs) = x : rejoin xs
rejoin [] = []

writeFileBinaryChanged :: FilePath -> String -> IO ()
writeFileBinaryChanged file x = do
    evaluate $ length x -- ensure we don't write out files with _|_ in them
    old <- ifM (doesFileExist file) (Just <$> readFileBinary' file) (pure Nothing)
    when (Just x /= old) $
        writeFileBinary file x

exclude :: [String]
exclude =
    ["Data.Foldable.Extra" -- because all their imports clash
    ]

excludeTests :: [String]
-- FIXME: Should probably generate these in another module
excludeTests =
    ["Data.List.NonEmpty.Extra" -- because !? clashes and is tested
    ]

hidden :: String -> [String]
hidden "Data.List.NonEmpty.Extra" = words
    "cons snoc sortOn union unionBy nubOrd nubOrdBy nubOrdOn (!?) foldl1' repeatedly"
hidden _ = []

notHidden :: String -> String -> Bool
notHidden mod fun = fun `notElem` hidden mod

addHiding :: String -> String
addHiding mod
  | xs@(_:_) <- hidden mod = mod ++ " hiding (" ++ intercalate ", " xs ++ ")"
  | otherwise = mod

validIdentifier xs =
    (take 1 xs == "(" || isName (takeWhile (/= '(') xs)) &&
    xs `notElem` ["module","Numeric"]

isName (x:xs) = isAlpha x && all (\x -> isAlphaNum x || x `elem` "_'") xs
isName _ = False

tweakTest x
    | Just x <- stripSuffix " == undefined" x =
        if not $ "\\" `isPrefixOf` x then
            (if "fileEq" `isInfixOf` x then "erroneousIO $ " else "erroneous $ ") ++ trim x
        else
            let (a,b) = breakOn "->" $ trim x
            in a ++ "-> erroneous $ " ++ trim (drop 2 b)
    | otherwise = x
