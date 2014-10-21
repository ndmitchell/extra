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
--
-- > \x -> fromLeft (Left  x) == x
-- > \x -> fromLeft (Right x) == undefined
fromLeft :: Either l r -> l
fromLeft (Left x) = x

-- | The 'fromRight' function extracts the element out of a 'Right' and
--   throws an error if its argument is 'Left'.
--   Much like 'fromJust', using this function in polished code is usually a bad idea.
--
-- > \x -> fromRight (Right x) == x
-- > \x -> fromRight (Left  x) == undefined
fromRight :: Either l r -> r
fromRight (Right x) = x

#if __GLASGOW_HASKELL__ < 708
-- | Test if an 'Either' value is the 'Left' constructor.
--   Provided as standard with GHC 7.8 and above.
isLeft :: Either l r -> Bool
isLeft Left{} = True; isLeft _ = False

-- | Test if an 'Either' value is the 'Right' constructor.
--   Provided as standard with GHC 7.8 and above.
isRight :: Either l r -> Bool
isRight Right{} = True; isRight _ = False
#endif

-- | Pull the value out of an 'Either' where both alternatives
--   have the same type.
--
-- > \x -> fromEither (Left x ) == x
-- > \x -> fromEither (Right x) == x
fromEither :: Either a a -> a
fromEither = either id id
