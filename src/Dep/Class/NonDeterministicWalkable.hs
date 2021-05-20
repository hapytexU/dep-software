{-# LANGUAGE FunctionalDependencies, Safe #-}

{-|
Module      : Dep.Class.NonDeterministicWalkable
Description : A module that exposes a type class that allows /non-deterministic/ "walking" through a data structure.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines a typeclass 'NonDeterministicWalkable' that specifies that one can walk through a non-deterministic
data structure where a certain step can result in zero, one, or more new states.
-}

module Dep.Class.NonDeterministicWalkable (
    -- * Non determinstic steps and walks
    NonDeterministicWalkable(nstep', nstep, nstepValues, nstepValues', nwalk, nwalkValues, allNstep, allNstep', allNwalk, allNstepValues, allNwalkValues)
  ) where

import Dep.Utils(toList')

import Control.Monad(foldM)

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

