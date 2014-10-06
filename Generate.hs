
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
    writeFile "Extra.hs" $ unlines $
        ["module Extra("] ++
        concat [["    -- * " ++ mod, "    " ++ unwords (map (++",") funs)] | (mod,funs,_) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ x | x <- mods]
    writeFile "Test.hs" $ unlines $
        ["module Test(main) where"] ++
        ["import " ++ x | x <- mods] ++
        ["main :: IO ()"
        ,"main = do"] ++
        ["  " ++ t | (_,_,ts) <- ifaces, t <- ts]


validIdentifier (x:xs) = isLower x && (x:xs) /= "module"
validIdentifier _ = False
