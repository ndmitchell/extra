
module Data.Either.Extra(module Data.Either, module Data.Either.Extra) where

import Data.Either

fromLeft (Left x) = x
fromRight (Right x) = x

isLeft Left{} = True; isLeft _ = False
isRight Right{} = True; isRight _ = False


loop :: (a -> Either a b) -> a -> b
loop act x = undefined

