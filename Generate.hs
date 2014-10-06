
module Generate(main) where

import Data.List.Extra
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
        let funcs = filter validIdentifier $ takeWhile (/= "where") $ words $ reps ',' ' ' $
                    unlines $ filter (not . isPrefixOf "--" . trim) $ lines src
        let tests = mapMaybe (stripPrefix "-- > ") $ lines src
        return (mod, funcs, tests)
    writeFile "src/Extra.hs" $ unlines $
        ["module Extra("] ++
        concat [["    -- * " ++ mod, "    " ++ unwords (map (++",") funs)] | (mod,funs,_) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ x | x <- mods]
    writeFile "src/Test.hs" $ unlines $
        ["{-# LANGUAGE ExtendedDefaultRules #-}"
        ,"module Test(main) where"
        ,"import TestUtil"
        ,"import Extra"
        ,"import Data.List"
        ,"import Test.QuickCheck"
        ,"default(Maybe Bool,Int,Double)"
        ,"main :: IO ()"
        ,"main = do"] ++
        ["  test " ++ show t ++ " $ " ++ t | (_,_,ts) <- ifaces, t <- ts] ++
        ["  putStrLn \"Success\""]


validIdentifier (x:xs) = isLower x && (x:xs) /= "module"
validIdentifier _ = False
