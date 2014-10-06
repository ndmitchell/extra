
module Data.Either.Extra(
    module Data.Either,
    isLeft, isRight, fromLeft, fromRight, fromEither
    ) where

import Data.Either

fromLeft (Left x) = x
fromRight (Right x) = x

isLeft Left{} = True; isLeft _ = False
isRight Right{} = True; isRight _ = False

fromEither :: Either a a -> a
fromEither = either id id
