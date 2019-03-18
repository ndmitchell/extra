-- | Extra functions for "Control.Monad.Except".  See
-- https://github.com/haskell/mtl/issues/60 for discussion.

module Control.Monad.Except.Extra(
    module Control.Monad.Except,
    tryError,
    withError,
    handleError
    ) where

import Control.Monad.Except (catchError, MonadError, throwError)

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | Modify the value (but not the type) of an error
withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) return

-- | MonadError analog of the 'handle' function.
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError
