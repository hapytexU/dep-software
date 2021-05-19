{-# LANGUAGE Safe #-}

{-|
Module      : Dep.Class.Mergeable
Description : A module to define logic to merge two values about.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines three-value logic, for example used in thruth tables, Karnaugh cards, etc.
-}

module Dep.Class.Mergeable (
    -- * The 'Mergeable' typeclass
    Mergeable(merge)
  ) where

-- | A typeclass that specifies that it is /sometimes/
-- possible to merge two values together into a new value.
class Mergeable a where
    -- | Try to merge two values into a new value. The result
    -- is wrapped in a 'Just' if it is possible; otherwise 'Nothing'
    -- is returned.
    merge
      :: a  -- ^ The first item to merge.
      -> a  -- ^ The second item to merge.
      -> Maybe a  -- ^ The result of the merge wrapped in a 'Just';
                  -- 'Nothing' if it is not possible to merge the two.

instance Mergeable (Maybe a) where
    merge x@(Just _) Nothing = Just x
    merge Nothing x@(Just _) = Just x
    merge Nothing Nothing = Just Nothing
    merge (Just _) (Just _) = Nothing
