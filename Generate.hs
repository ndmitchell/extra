
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
    mods <- return $ filter (isSuffixOf ".Extra") $ map strip $ lines src
    ifaces <- forM mods $ \mod -> do
        src <- readFile $ joinPath ("src" : split (== '.') mod) <.> "hs"
        let funcs = filter validIdentifier $ takeWhile (/= "where") $
                    words $ reps ',' ' ' $ drop1 $ dropWhile (/= '(') $
                    unlines $ filter (not . isPrefixOf "--" . strip) $ lines src
        let tests = mapMaybe (stripPrefix "-- > ") $ lines src
        return (mod, funcs, tests)
    writeFileBinary "src/Extra.hs" $ unlines $
        ["module Extra("] ++
        concat [["    -- * " ++ mod, "    " ++ unwords (map (++",") funs)] | (mod,funs,_) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ x | x <- mods]
    writeFileBinary "src/Test.hs" $ unlines $
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


validIdentifier (x:xs) = (x == '(' || isLower x) && (x:xs) /= "module"
validIdentifier _ = False
