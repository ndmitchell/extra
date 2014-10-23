
-- | Extra functions for working with tuples.
--   Some of these functions are available in the "Control.Arrow" module,
--   but here are available specialised to pairs.
module Data.Tuple.Extra(
    module Data.Tuple,
    -- * Specialised 'Arrow' functions
    first, second, (***), (&&&),
    -- * More pair operations
    dupe, both,
    -- * Extract from a triple
    fst3, snd3, thd3
    ) where

import Data.Tuple
import qualified Control.Arrow as Arrow

infixr 3 ***, &&&

-- | Update the first component of a pair.
first :: (a -> a') -> (a, b) -> (a', b)
first = Arrow.first

-- | Update the second component of a pair.
second :: (b -> b') -> (a, b) -> (a, b')
second = Arrow.second

(***) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(***) = (Arrow.***)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) = (Arrow.&&&)

dupe :: a -> (a,a)
dupe x = (x,x)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c
