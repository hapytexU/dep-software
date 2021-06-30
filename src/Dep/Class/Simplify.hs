{-# LANGUAGE Safe #-}

module Dep.Class.Simplify (
    -- * A typeclass to specify that values of the type can be simplified.
    Simplify(simplify)
  ) where

-- | A typeclass that specifies that the the values of the given type sometimes
-- can be simplfied. The 'simplify' operation is /idempotent/: calling it multiple
-- times has the same effect as calling it once.
class Simplify a where
  -- | A function to simplify the given object to a potentially simplier object.
  -- This function should be /idempotent/: calling 'simplify' multiple times
  -- has the same effect as calling 'simplify' once.
  simplify
    :: a  -- ^ The given object to simplify.
    -> a  -- ^ The correspoding simplified relevant.
  {-# MINIMAL simplify #-}
