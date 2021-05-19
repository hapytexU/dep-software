{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, Safe #-}

{-|
Module      : Dep.Class.Walkable
Description : A module that exposes a type class that allows "walking" through a data structure.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines a typeclass 'Walkable' that specifies that one can walk through a deterministic
data structure for a given type of "steps".
-}

module Dep.Class.Walkable (
    -- * Deterministic steps and walks
    Walkable(step, walk, allStep, allWalk, stepValues, stepValues', allStepValues, allStepValues', walkValues', allWalkValues)
  ) where

import Dep.Utils(toList')

import Data.Foldable(toList)

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
