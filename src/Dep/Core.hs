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
    BFunc, BFunc1, BFunc2, BFunc3, BFunc4
  , Mergeable(merge)
  , Walkable(step, walk), NonDeterministicWalkable(nstep', nstep, allnstep', allnstep)
  ) where

-- | A function that maps a list of 'Bool's to a single 'Bool'.
type BFunc = [Bool] -> Bool

type BoolFunc a = Bool -> a

-- | The type alias for a function that maps a 'Bool' to another 'Bool', only four functions are possible.
type BFunc1 = BoolFunc Bool

-- | The type alias for a function that maps two 'Bool's to another 'Bool', sixteen functions are possible.
type BFunc2 = BoolFunc BFunc1

-- | The type alias for a function that maps three 'Bool's to another 'Bool', 256 functions are possible.
type BFunc3 = BoolFunc BFunc2

-- | The type alias for a function that maps four 'Bool's to another 'Bool', 65536 functions are possible.
type BFunc4 = BoolFunc BFunc3

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

class Walkable f step | f -> step where
  -- | Take one step with the given step parameter, and return an object with the same
  -- type.
  step
    :: f a  -- ^ The original object where we will make a step.
    -> step  -- ^ The given step we take on the given data structure.
    -> f a  -- ^ The result of an object when we take one step.
  step item stp = walk item [stp]

  -- | Take a sequence of steps with the given list of steps.
  walk :: Foldable g
    => f a  -- ^ The original object where we will make a step.
    -> g step  -- ^ The given foldable of steps we take on the given data structure.
    -> f a  -- ^ The result of an object when we take one step.
  walk = foldl step
  {-# MINIMAL step | walk #-}

class NonDeterministicWalkable f step | f -> step where
  -- | Take a non-deterministic step that can result in multiple outcomes.
  -- One can specify a tail to make concatenating of lists more efficient.
  nstep'
    :: f a  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of tail elements added to the result.
    -> [f a]  -- ^ The list of the possible states with the new step.
  nstep' x dx = (nstep x dx ++)

  -- | Take a non-deterministic step that can result in multiple outcomes.
  nstep
    :: f a  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of the possible states with the new step.
  nstep x dx = nstep' x dx []

  -- | Take the same non-deterministic step for all initial states.
  -- This can result in multiple outcomes. One can specify a tail to
  -- make concatenating of lists more efficient.
  allnstep' :: Foldable g
    => g (f a)  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of tail elements added to the result.
    -> [f a]  -- ^ The list of the possible states with the new step.
  allnstep' xs dx tl = foldr (`nstep'` dx) tl xs

  -- | Take the same non-deterministic step for all initial states.
  -- This can result in multiple outcomes.
  allnstep :: Foldable g
    => g (f a)  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of the possible states with the new step.
  allnstep xs dx = allnstep' xs dx []

  -- nwalk' :: Foldable g => f a -> g step -> [f a] -> [f a]
  -- nwalk' x dxs tl = foldr (nstep' x) tl dxs
  {-# MINIMAL nstep' | nstep #-}

instance Mergeable (Maybe a) where
    merge x@(Just _) Nothing = Just x
    merge Nothing x@(Just _) = Just x
    merge Nothing Nothing = Just Nothing
    merge (Just _) (Just _) = Nothing
