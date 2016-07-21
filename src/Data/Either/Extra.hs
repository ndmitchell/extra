{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.Either" with extra operations, particularly
--   to quickly extract from inside an 'Either'. Some of these operations are
--   partial, and should be used with care in production-quality code.
module Data.Either.Extra(
    module Data.Either,
    isLeft, isRight, fromLeft, fromRight, fromEither
    ) where

import Data.Either


foo = error __GLASGOW_HASKELL__

#if __GLASGOW_HASKELL__ < 801

-- | Return the contents of a 'Left'-value or a default value otherwise.
--
-- > fromLeft 1 (Left 3) == 3
-- > fromLeft 1 (Right "foo") == 1
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

-- | Return the contents of a 'Right'-value or a default value otherwise.
--
-- > fromRight 1 (Right 3) == 3
-- > fromRight 1 (Left "foo") == 1
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

#endif

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
