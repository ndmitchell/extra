{-# LANGUAGE CPP, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.Version" with extra utilities.
--   The package also exports the existing "Data.Version" functions.
module Data.Version.Extra(
    module Data.Version,
    makeVersion, readVersion
    ) where

import Partial
import Data.Version
import Data.List.Extra
import Text.ParserCombinators.ReadP


#if __GLASGOW_HASKELL__ < 710

-- | Construct tag-less 'Version'
--
-- > showVersion (makeVersion [1,2,3]) == "1.2.3"
makeVersion :: [Int] -> Version
makeVersion b = Version b []

#endif

-- | Read a 'Version' or throw an exception.
--
-- > \x -> readVersion (showVersion x) == x
-- > readVersion "hello" == undefined
readVersion :: Partial => String -> Version
readVersion s =
    case [ x | (x,"") <- readP_to_S parseVersion $ trimEnd s] of
        [x] -> x
        []  -> error "Data.Version.Extra.readVersion: no parse"
        _   -> error "Data.Version.Extra.readVersion: ambiguous parse"
