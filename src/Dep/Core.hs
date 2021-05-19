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
