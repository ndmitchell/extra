
module Main(main) where

import Data.List.Extra
import Control.Monad
import System.FilePath
import Data.Char


main :: IO ()
main = do
    src <- readFile "extra.cabal"
    mods <- return $ filter (isSuffixOf ".Extra") $ map trim $ lines src
    ifaces <- forM mods $ \mod -> do
        src <- readFile $ joinPath (wordsBy (== '.') mod) <.> "hs"
        let funcs = filter validIdentifier $ takeWhile (/= "where") $ words $ reps ',' ' ' $
                    unlines $ filter (not . isPrefixOf "--" . trim) $ lines src
        return (mod, funcs)
    writeFile "Extra.hs" $ unlines $
        ["module Extra("] ++
        concat [["    -- * " ++ mod, "    " ++ unwords (map (++",") funs)] | (mod,funs) <- ifaces] ++
        ["    ) where"
        ,""] ++
        ["import " ++ x | x <- mods]

validIdentifier (x:xs) = isLower x && (x:xs) /= "module"
validIdentifier _ = False
