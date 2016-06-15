{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators, GADTs, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.Typeable" with extra functions available in later GHC versions.
--   The package also exports the existing "Data.Typeable" functions.
module Data.Typeable.Extra(
    typeRep, (:~:)(..), Proxy(..),
    module Data.Typeable
    ) where

import Data.Typeable

#if __GLASGOW_HASKELL__ < 708

import Data.Ix
import Data.Monoid
import Control.Monad
import Control.Applicative


-- | Takes a value of type @a@ and returns a concrete representation
--   of that type.
--
-- > typeRep (Proxy :: Proxy Int) == typeOf (1 :: Int)
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)


infix 4 :~:

-- | Propositional equality. If @a :~: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :~: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
data a :~: b where
    Refl :: a :~: a

deriving instance Eq   (a :~: b)
deriving instance Show (a :~: b)
deriving instance Ord  (a :~: b)

instance a ~ b => Read (a :~: b) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl, s) | ("Refl",s) <- lex r ])

instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = errorWithoutStackTrace "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

instance a ~ b => Bounded (a :~: b) where
  minBound = Refl
  maxBound = Refl



-- | A canonical proxy type
data Proxy t = Proxy

-- It's common to use (undefined :: Proxy t) and (Proxy :: Proxy t)
-- interchangeably, so all of these instances are hand-written to be
-- lazy in Proxy arguments.

instance Eq (Proxy s) where
  _ == _ = True

instance Ord (Proxy s) where
  compare _ _ = EQ

instance Show (Proxy s) where
  showsPrec _ _ = showString "Proxy"

instance Read (Proxy s) where
  readsPrec d = readParen (d > 10) (\r -> [(Proxy, s) | ("Proxy",s) <- lex r ])

errorWithoutStackTrace = error

instance Enum (Proxy s) where
    succ _               = errorWithoutStackTrace "Proxy.succ"
    pred _               = errorWithoutStackTrace "Proxy.pred"
    fromEnum _           = 0
    toEnum 0             = Proxy
    toEnum _             = errorWithoutStackTrace "Proxy.toEnum: 0 expected"
    enumFrom _           = [Proxy]
    enumFromThen _ _     = [Proxy]
    enumFromThenTo _ _ _ = [Proxy]
    enumFromTo _ _       = [Proxy]

instance Ix (Proxy s) where
    range _           = [Proxy]
    index _ _         = 0
    inRange _ _       = True
    rangeSize _       = 1

instance Bounded (Proxy s) where
    minBound = Proxy
    maxBound = Proxy

instance Monoid (Proxy s) where
    mempty = Proxy
    mappend _ _ = Proxy
    mconcat _ = Proxy

instance Functor Proxy where
    fmap _ _ = Proxy
    {-# INLINE fmap #-}

instance Applicative Proxy where
    pure _ = Proxy
    {-# INLINE pure #-}
    _ <*> _ = Proxy
    {-# INLINE (<*>) #-}

instance Alternative Proxy where
    empty = Proxy
    {-# INLINE empty #-}
    _ <|> _ = Proxy
    {-# INLINE (<|>) #-}

instance Monad Proxy where
    return = pure
    _ >>= _ = Proxy
    {-# INLINE (>>=) #-}

instance MonadPlus Proxy where
    mzero = empty
    mplus = (<|>)

#endif
