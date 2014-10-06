-- FIXME: Todo

module Control.Monad.Extra(module Control.Monad.Extra) where

import Control.Monad
import Data.Maybe


whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

unit :: m () -> m ()
unit = id



partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

concatZipWithM :: Monad m => (a -> b -> m [c]) -> [a] -> [b] -> m [c]
concatZipWithM f xs ys = liftM concat $ zipWithM f xs ys

listM' :: Monad m => [a] -> m [a]
listM' x = length x `seq` return x


---------------------------------------------------------------------
-- Control.Monad

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

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = liftM catMaybes $ mapM f xs

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

notM :: Functor m => m Bool -> m Bool
notM = fmap not

(||^), (&&^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = do a <- a; if a then return True else b
(&&^) a b = do a <- a; if a then b else return False


findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
    v <- p x
    if v then return $ Just x else findM p xs

findJustM = undefined
