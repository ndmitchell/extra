{-# LANGUAGE TupleSections #-}

-- | Extra functions for working with pairs and triples.
--   Some of these functions are available in the "Control.Arrow" module,
--   but here are available specialised to pairs. Some operations work on triples.
module Data.Tuple.Extra(
    module Data.Tuple,
    -- * Specialised 'Arrow' functions
    first, second, (***), (&&&),
    -- * More pair operations
    dupe, both,
    -- * Monadic versions
    firstM, secondM,
    -- * Operations on triple
    fst3, snd3, thd3,
    first3, second3, third3,
    curry3, uncurry3
    ) where

import Data.Tuple
import qualified Control.Arrow as Arrow

infixr 3 ***, &&&

-- | Update the first component of a pair.
--
-- > first succ (1,"test") == (2,"test")
first :: (a -> a') -> (a, b) -> (a', b)
first = Arrow.first

-- | Update the second component of a pair.
--
-- > second reverse (1,"test") == (1,"tset")
second :: (b -> b') -> (a, b) -> (a, b')
second = Arrow.second

-- | Update the first component of a pair.
--
-- > firstM (\x -> [x-1, x+1]) (1,"test") == [(0,"test"),(2,"test")]
firstM :: Functor m => (a -> m a') -> (a, b) -> m (a', b)
firstM f ~(a,b) = (,b) <$> f a

-- | Update the second component of a pair.
--
-- > secondM (\x -> [reverse x, x]) (1,"test") == [(1,"tset"),(1,"test")]
secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f ~(a,b) = (a,) <$> f b

-- | Given two functions, apply one to the first component and one to the second.
--   A specialised version of 'Control.Arrow.***'.
--
-- > (succ *** reverse) (1,"test") == (2,"tset")
(***) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(***) = (Arrow.***)

-- | Given two functions, apply both to a single argument to form a pair.
--   A specialised version of 'Control.Arrow.&&&'.
--
-- > (succ &&& pred) 1 == (2,0)
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) = (Arrow.&&&)

-- | Duplicate a single value into a pair.
--
-- > dupe 12 == (12, 12)
dupe :: a -> (a,a)
dupe x = (x,x)

-- | Apply a single function to both components of a pair.
--
-- > both succ (1,2) == (2,3)
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x,y) = (f x, f y)

-- | Extract the 'fst' of a triple.
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- | Extract the 'snd' of a triple.
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

-- | Extract the final element of a triple.
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

-- | Converts an uncurried function to a curried function.
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c


-- | Update the first component of a triple.
--
-- > first3 succ (1,1,1) == (2,1,1)
first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f ~(a,b,c) = (f a,b,c)

-- | Update the second component of a triple.
--
-- > second3 succ (1,1,1) == (1,2,1)
second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f ~(a,b,c) = (a,f b,c)

-- | Update the third component of a triple.
--
-- > third3 succ (1,1,1) == (1,1,2)
third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 f ~(a,b,c) = (a,b,f c)
