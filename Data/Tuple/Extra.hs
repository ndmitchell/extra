
module Data.Tuple.Extra(module Data.Tuple, module Data.Tuple.Extra) where

import Data.Tuple

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

concat3 :: [([a],[b],[c])] -> ([a],[b],[c])
concat3 xs = (concat a, concat b, concat c)
    where (a,b,c) = unzip3 xs

concat2 :: [([a],[b])] -> ([a],[b])
concat2 xs = (concat a, concat b)
    where (a,b) = unzip xs

dupe x = (x,x)
