{-# LANGUAGE FunctionalDependencies, Safe #-}

{-|
Module      : Dep.Core
Description : A module that defines utility data structures, typeclasses and type aliasses.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines some aliasses for boolean functions with one, two, three, and four parameters, and
defines a 'Mergeable' typeclass that is used to optionally merge two values into a new value.
-}

module Dep.Core (
    -- * Boolean functions
    BFunc, BFunc1, BFunc2, BFunc3, BFunc4
    -- * Merging two items
  , Mergeable(merge)
    -- * Determine the opposite value
  , Opposite(opposite)
  ) where

-- | A function that maps a list of 'Bool's to a single 'Bool'.
type BFunc = [Bool] -> Bool

-- | The type alias for a function that maps a 'Bool' to another 'Bool', only four functions are possible.
type BFunc1 = Bool -> Bool

-- | The type alias for a function that maps two 'Bool's to another 'Bool', sixteen functions are possible.
type BFunc2 = Bool -> Bool -> Bool

-- | The type alias for a function that maps three 'Bool's to another 'Bool', 256 functions are possible.
type BFunc3 = Bool -> Bool -> Bool -> Bool

-- | The type alias for a function that maps four 'Bool's to another 'Bool', 65536 functions are possible.
type BFunc4 = Bool -> Bool -> Bool -> Bool -> Bool

-- | A typeclass where the values of its members have an opposite element from the same type.
class Opposite a where
  -- | A function that determines the opposite value.
  opposite
    :: a  -- ^ The given item to determine the opposite from.
    -> a  -- ^ The opposite of the given value.

instance Opposite b => Opposite ((->) a b) where
  opposite = fmap opposite

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
