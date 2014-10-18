{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Data.Either.Extra(
    module Data.Either,
    isLeft, isRight, fromLeft, fromRight, fromEither
    ) where

import Data.Either

-- | The 'fromLeft' function extracts the element out of a 'Left' and
--   throws an error if its argument is 'Right'.
--   Much like 'fromJust', using this function in polished code is usually a bad idea.
fromLeft :: Either l r -> l
fromLeft (Left x) = x

-- | The 'fromRight' function extracts the element out of a 'Right' and
--   throws an error if its argument is 'Left'.
--   Much like 'fromJust', using this function in polished code is usually a bad idea.
fromRight :: Either l r -> r
fromRight (Right x) = x

#if __GLASGOW_HASKELL__ < 708
isLeft Left{} = True; isLeft _ = False
isRight Right{} = True; isRight _ = False
#endif

-- | Pull the value out of an 'Either' where both alternatives
--   have the same type.
fromEither :: Either a a -> a
fromEither = either id id
