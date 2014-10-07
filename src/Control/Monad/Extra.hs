
-- If you need a wider selection of monad loops and list generalisations,
-- see <http://hackage.haskell.org/package/monad-loops>
module Control.Monad.Extra(
    module Control.Monad,
    whenJust,
    unit,
    partitionM, concatMapM,
    loopM, whileM,
    ifM, notM, (||^), (&&^), orM, andM, anyM, allM,
    findM, firstJustM
    ) where

import Control.Monad
import Control.Applicative

-- General utilities

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

unit :: m () -> m ()
unit = id

-- Data.List for Monad

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

-- Looping

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x -> loopM act x
        Right v -> return v

whileM :: Monad m => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act

-- Booleans

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

notM :: Functor m => m Bool -> m Bool
notM = fmap not

-- > Just False &&^ undefined == Just False
-- > Just True &&^ Just True == Just True
(||^), (&&^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (return True) b
(&&^) a b = ifM a b (return False)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p [] = return False
anyM p (x:xs) = ifM (p x) (return True) (anyM p xs)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p [] = return True
allM p (x:xs) = ifM (p x) (allM p xs) (return False)

orM :: Monad m => [m Bool] -> m Bool
orM = anyM id

andM :: Monad m => [m Bool] -> m Bool
andM = allM id

-- Searching

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM p [] = return Nothing
firstJustM p (x:xs) = maybe (firstJustM p xs) (return . Just) =<< p x
