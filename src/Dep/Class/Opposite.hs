{-# LANGUAGE DefaultSignatures, Safe, TypeFamilies #-}

{-|
Module      : Dep.Class.Walkable
Description : A module that exposes a type class that specifies that for an item of a specific type, there is an item that is the opposite of the given item.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines a typeclass 'Opposite' that specifies that each value of
a specific type has an opposite value of the same type.
-}

module Dep.Class.Opposite (
    -- * A typeclass to determine the opposite value.
    Opposite(opposite)
  ) where

-- | A typeclass where the values of its members have an opposite element from the same type.
class Opposite a where
  -- | A function that determines the opposite value.
  opposite
    :: a  -- ^ The given item to determine the opposite from.
    -> a  -- ^ The opposite of the given value.
  default opposite :: (Functor f, Opposite b, a ~ f b) => a -> a
  opposite = fmap opposite

instance Opposite Bool where
  opposite = not

instance Opposite b => Opposite ((->) a b)

instance Opposite a => Opposite [a]

instance Opposite a => Opposite (Maybe a)
