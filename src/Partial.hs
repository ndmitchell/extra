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

-- | A constraint synonym which denotes that the function is partial, and will
--   (on GHC 8.* and up) produce a stack trace on failure.
--   You may mark your own non-total functions as Partial, if necessary, and this
--   will ensure that they produce useful stack traces.
#if __GLASGOW_HASKELL__ >= 800
type Partial = HasCallStack
#else
type Partial = (() :: Constraint)
#endif
