{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE CPP             #-}

-- | ConstraintKind synonym for marking partial functions
module Partial(Partial) where
-- Originally taken from the @safe@ package

-- GHC has changed its opinion on the location a few times
-- v0: GHC 7.4.1, has ConstraintKinds
-- v1: GHC 7.10.2, base 4.8.1.0 = CallStack
-- v2: GHC 8.0.1, base 4.9.0.0 = HasCallStack

-- We never support GHC 7.10.2 style because that requires users to pass the FlexibleContexts
-- extension

#if __GLASGOW_HASKELL__ >= 800
import GHC.Stack
#else
import GHC.Exts
#endif

-- | A constraint which documents that a function is partial, and on GHC 8.0
--   and above produces a stack trace on failure. For example:
--
-- @
-- myHead :: 'Partial' => [a] -> a
-- myHead [] = error \"bad\"
-- myHead (x:xs) = x
-- @
--
--   When using 'Partial' with GHC 7.8 or below you need to enable the
--   language feature @ConstraintKinds@, e.g. @{-\# LANGUAGE ConstraintKinds \#-}@
--   at the top of the file.
#if __GLASGOW_HASKELL__ >= 800
type Partial = HasCallStack
#else
type Partial = (() :: Constraint)
#endif
