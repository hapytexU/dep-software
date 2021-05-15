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
    -- * Deterministic and non-deterministic walks.
  , Walkable(step, walk, allStep, allWalk, stepValues, stepValues', allStepValues, allStepValues', walkValues', allWalkValues)
  , NonDeterministicWalkable(nstep', nstep, nstepValues, nstepValues', nwalk, nwalkValues, allNstep, allNstep', allNwalk, allNstepValues, allNwalkValues)
  ) where

import Control.Monad(foldM)

import Data.Foldable(toList)

import Dep.Utils(toList')

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

-- | A typeclass that specifies that we can walk through a datastructure with steps.
class Walkable f step | f -> step where
  -- | Take one step with the given step parameter, and return an object with the same
  -- type.
  step
    :: f a  -- ^ The original object where we will make a step.
    -> step  -- ^ The given step we take on the given data structure.
    -> f a  -- ^ The result of an object when we take one step.
  step item stp = walk item [stp]

  -- | Obtain the values of the item after making a step.
  stepValues' :: Foldable f
    => f a -- ^ The initial state.
    -> step -- ^ The step that we take to move to another state.
    -> [a] -- ^ A list of tail elements that can be added to the result.
    -> [a] -- ^ The list of values from the modified state together with the given tail.
  stepValues' item stp = toList' (step item stp)

  -- | Obtain the values of the item after making a step.
  stepValues :: Foldable f
    => f a -- ^ The initial state.
    -> step -- ^ The step that we take to move to another state.
    -> [a] -- ^ The list of values from the modified state.
  stepValues item stp = stepValues' item stp []

  -- | Apply the same step for all the items in the given collection (functor) of items.
  allStep :: Functor g
     => g (f a)  -- ^ The collection of items on which we apply a step.
     -> step  -- ^ The given step that will be applied.
     -> g (f a) -- ^ A collection of items that are the result of making a step for each input item.
  allStep = flip (fmap . flip step)

  -- | Obtain the values of the items after making the same step for all items.
  allStepValues' :: (Foldable f, Foldable g)
    => g (f a) -- ^ The initial states.
    -> step -- ^ The step that we take to move to another state.
    -> [a] -- ^ A list of tail elements that can be added to the result.
    -> [a] -- ^ The list of values from the modified state together with the given tail.
  allStepValues' items stp tl = foldr (toList' . (`step` stp)) tl items

  -- | Obtain the values of the items after making the same step for all items.
  allStepValues :: (Foldable f, Foldable g)
    => g (f a) -- ^ The initial states.
    -> step -- ^ The step that we take to move to another state.
    -> [a] -- ^ The list of values from the modified state.
  allStepValues items stp = allStepValues' items stp []

  -- | Take a sequence of steps with the given list of steps.
  walk :: Foldable g
    => f a  -- ^ The original object where we will make a step.
    -> g step  -- ^ The given foldable of steps we take on the given data structure.
    -> f a  -- ^ The result of an object when we take one step.
  walk = foldl step

  -- | Take a sequence of steps with the given list of steps and return the values out of the target state.
  walkValues' :: (Foldable f, Foldable g)
    => f a  -- ^ The initial state for which we will make a walk.
    -> g step -- ^ A sequene of steps that describe the walk.
    -> [a] -- ^ A list of tail elements that can be added at the end of the list.
    -> [a] -- ^ The values found after making the a walk with the given 'Walkable' in the 'Foldable'.
  walkValues' x = toList' . walk x

  -- | Obtain all the values the 'Foldable' f spans after performing the
  -- same ('Foldable') of steps on all the given intial states.
  allWalkValues :: (Foldable f, Foldable g, Functor g, Foldable h)
    => g (f a)  -- ^ The given collection of initial states over which a fold is made.
    -> h step  -- ^ A 'Foldable' of steps that we will take for all states.
    -> [a] -- ^ The corresponding values that are wrapped in the target states.
  allWalkValues x = concatMap toList . allWalk x

  -- Apply the same walk to all elements in the collection (functor).
  allWalk :: (Functor g, Foldable h)
    => g (f a) -- ^ The given collection of initial objects.
    -> h step -- ^ The given foldable of steps that we take.
    -> g (f a) -- ^ The result collection of items after applying the steps.
  allWalk = foldl allStep
  {-# MINIMAL step | walk #-}

-- | A typeclass that specifies that we can walk through the given datastructure
-- where steps can be non-deterministic and thus result in /multiple/ possible states.
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

  -- | Obtain the values stored in the 'Foldable' after making a non-deterministic
  -- step (which can result in zero, one or more new states).
  nstepValues' :: Foldable f
    => f a  -- ^ The initial state.
    -> step  -- ^ The non-deterministic step we take.
    -> [a]  -- ^ The list of tail elements that will be added to the list.
    -> [a]  -- ^ A list of values wrapped in the 'Foldable's after making a non-deterministic step.
  nstepValues' st stp = flip (foldr toList') (nstep st stp)

  -- | Obtain the values stored in the 'Foldable' after making a non-deterministic
  -- step (which can result in zero, one or more new states).
  nstepValues :: Foldable f
    => f a  -- ^ The initial state.
    -> step  -- ^ The non-deterministic step we take.
    -> [a]  -- ^ A list of values wrapped in the 'Foldable's after making a non-deterministic step.
  nstepValues st stp = nstepValues' st stp []


  -- | Take the same non-deterministic step for all initial states.
  -- This can result in multiple outcomes. One can specify a tail to
  -- make concatenating of lists more efficient.
  allNstep' :: Foldable g
    => g (f a)  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of tail elements added to the result.
    -> [f a]  -- ^ The list of the possible states with the new step.
  allNstep' xs dx tl = foldr (`nstep'` dx) tl xs

  -- | Take the same non-deterministic step for all initial states.
  -- This can result in multiple outcomes.
  allNstep :: Foldable g
    => g (f a)  -- ^ The given initial state where we make a non-determinstic step.
    -> step  -- ^ The step that we make, such step can result in zero, one or more new states.
    -> [f a]  -- ^ The list of the possible states with the new step.
  allNstep xs dx = allNstep' xs dx []

  -- | Make a non-deterministic step for each state in the collection,
  -- and collect the data of the 'Foldable's after making.
  allNstepValues :: Foldable f
    => [f a] -- ^ The initial states for te walk.
    -> step -- ^ The non-deterministic step we make.
    -> [a] -- ^ The resulting (possible) values that are stored in the final foldables.
  allNstepValues inis stps = inis >>= (`nstepValues` stps)

  -- Make a walk with non-deterministic steps that can result in multiple paths.
  nwalk :: Foldable g
    => f a -- ^ The initial state of the walk.
    -> g step -- ^ A 'Foldable' of steps that we take.
    -> [f a] -- ^ The resulting (possible) states after taking the walk.
  nwalk = foldM nstep

  -- Make a walk with non-deterministic steps that can result in multiple paths.
  -- After the walk all the states will report the values wrapped in the 'Foldable'.
  nwalkValues :: (Foldable f, Foldable g)
    => f a -- ^ The initial state of the walk.
    -> g step -- ^ A 'Foldable' of steps that we take.
    -> [a] -- ^ The resulting (possible) values that are stored in the final foldables.
  nwalkValues ini stps = foldr toList' [] (foldM nstep ini stps)


  -- | Make a non-deterministic walk with a collection of states that can result in multiple paths.
  allNwalk :: Foldable g
    => [f a] -- ^ The initial states for te walk.
    -> g step -- ^ A 'Foldable' of steps that we will take.
    -> [f a] -- The resulting list of possible states.
  allNwalk = (. flip nwalk) . (>>=)

  -- | Make a non-deterministic walk with a collection of states that can result in multiple paths.
  allNwalkValues :: (Foldable f, Foldable g)
    => [f a] -- ^ The initial states for te walk.
    -> g step -- ^ A 'Foldable' of steps that we will take.
    -> [a] -- ^ The resulting (possible) values that are stored in the final foldables.
  allNwalkValues inis stps = inis >>= (`nwalkValues` stps)
  {-# MINIMAL nstep' | nstep #-}

instance Mergeable (Maybe a) where
    merge x@(Just _) Nothing = Just x
    merge Nothing x@(Just _) = Just x
    merge Nothing Nothing = Just Nothing
    merge (Just _) (Just _) = Nothing
