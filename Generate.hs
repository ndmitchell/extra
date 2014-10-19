
module Generate(main) where

import Data.List.Extra
import System.IO.Extra
import Control.Monad
import System.FilePath
import Data.Char
import Data.Maybe


main :: IO ()
main = do
    src <- readFile "extra.cabal"
    mods <- return $ filter (isSuffixOf ".Extra") $ map trim $ lines src
    ifaces <- forM mods $ \mod -> do
        src <- readFile $ joinPath ("src" : split (== '.') mod) <.> "hs"
        let funcs = filter validIdentifier $ takeWhile (/= "where") $
                    words $ replace "," " " $ drop1 $ dropWhile (/= '(') $
                    unlines $ filter (not . isPrefixOf "--" . trim) $ lines src
        let tests = mapMaybe (stripPrefix "-- > ") $ lines src
        return (mod, funcs, tests)
    writeFileBinary "src/Extra.hs" $ unlines $
        ["-- | This module documents all the functions available in this package."
        ,"--"
        ,"--   Most users should import the specific modules (e.g. @\"Data.List.Extra\"@), which"
        ,"--   also reexport their non-@Extra@ modules (e.g. @\"Data.List\"@)."
        ,"module Extra("] ++
        concat [ ["    -- * " ++ mod
                 ,"    -- | Extra functions available in @" ++ show mod ++ "@."
                 ,"    " ++ unwords (map (++",") funs)]
               | (mod,funs,_) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ x | x <- mods]
    writeFileBinary "test/TestGen.hs" $ unlines $
        ["{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}"
        ,"module TestGen(tests) where"
        ,"import TestUtil"
        ,"default(Maybe Bool,Int,Double)"
        ,"tests :: IO ()"
        ,"tests = do"] ++
        ["    testGen " ++ show t ++ " $ " ++ tweakTest t | (_,_,ts) <- ifaces, t <- ts]


validIdentifier xs =
    (take 1 xs == "(" || isName xs) &&
    xs `notElem` ["module","Numeric"]

isName (x:xs) = isAlpha x && all (\x -> isAlphaNum x || x `elem` "_'") xs
isName _ = False

tweakTest x | Just x <- stripSuffix " == error" x = "erroneous $ " ++ x
            | otherwise = x
