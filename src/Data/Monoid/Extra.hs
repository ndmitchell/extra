
-- | Extra functions for working with monoids.
module Data.Monoid.Extra(
    module Data.Monoid,
    -- * Extra operations
    mwhen,

    -- * Kleisli endomorphisms
    KEndo(KEndo, appKEndo)
    ) where

import Data.Monoid
import Control.Monad

-- | Like 'Control.Monad.when', but operating on a 'Monoid'. If the first argument
--   is 'True' returns the second, otherwise returns 'mempty'.
--
-- > mwhen True  "test" == "test"
-- > mwhen False "test" == ""
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty

-- | The intersection of 'Data.Monoid.Endo' and 'Control.Arrow.Kleisli'. This
--   type provides a 'Monoid' instance for composition of monadic actions
--   @a -> m a@.
newtype KEndo m a = KEndo { appKEndo :: a -> m a }

instance (Monad m) => Semigroup (KEndo m a) where
    -- | Left-to-right composition
    KEndo f <> KEndo g = KEndo (f <=< g)

instance (Monad m) => Monoid (KEndo m a) where
    mempty = KEndo pure

