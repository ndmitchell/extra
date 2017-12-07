{-# LANGUAGE CPP, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.Either" with extra operations, particularly
--   to quickly extract from inside an 'Either'. Some of these operations are
--   partial, and should be used with care in production-quality code.
module Data.Either.Extra(
    module Data.Either,
    isLeft, isRight, fromLeft, fromRight, fromEither,
    fromLeft', fromRight',
    eitherToMaybe, maybeToEither,
    ) where

import Data.Either
import Partial


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


-- | The 'fromLeft'' function extracts the element out of a 'Left' and
--   throws an error if its argument is 'Right'.
--   Much like 'fromJust', using this function in polished code is usually a bad idea.
--
-- > \x -> fromLeft' (Left  x) == x
-- > \x -> fromLeft' (Right x) == undefined
fromLeft' :: Partial => Either l r -> l
fromLeft' (Left x) = x
fromLeft' _ = error "fromLeft', given a Right"

-- | The 'fromRight'' function extracts the element out of a 'Right' and
--   throws an error if its argument is 'Left'.
--   Much like 'fromJust', using this function in polished code is usually a bad idea.
--
-- > \x -> fromRight' (Right x) == x
-- > \x -> fromRight' (Left  x) == undefined
fromRight' :: Partial => Either l r -> r
fromRight' (Right x) = x
fromRight' _ = error "fromRight', given a Left"


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


-- | Given a 'Maybe', convert it to an 'Either', providing a suitable
--   value for the 'Left' should the value be 'Nothing'.
--
-- > \a b -> maybeToEither a (Just b) == Right b
-- > \a -> maybeToEither a Nothing == Left a
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing = Left a

-- | Given an 'Either', convert it to a 'Maybe', where 'Left' becomes 'Nothing'.
--
-- > \x -> eitherToMaybe (Left x) == Nothing
-- > \x -> eitherToMaybe (Right x) == Just x
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
