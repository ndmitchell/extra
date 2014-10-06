{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Data.Either.Extra(
    module Data.Either,
    isLeft, isRight, fromLeft, fromRight, fromEither
    ) where

import Data.Either

fromLeft (Left x) = x
fromRight (Right x) = x

#if __GLASGOW_HASKELL__ < 708
isLeft Left{} = True; isLeft _ = False
isRight Right{} = True; isRight _ = False
#endif

fromEither :: Either a a -> a
fromEither = either id id
