{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators, GADTs #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module extends "Data.Typeable" with extra functions available in later GHC versions.
--   The package also exports the existing "Data.Typeable" functions.
module Data.Typeable.Extra(
    typeRep, (:~:)(..), Proxy(..),
    module Data.Typeable
    ) where

import Data.Typeable

#if __GLASGOW_HASKELL__ < 708

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

-- | A canonical proxy type
data Proxy t = Proxy

#endif
