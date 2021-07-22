module Dep.Data.FiniteStateMachine.Mealy where

import Data.HashMap(Map)

data MealyFSM st it ot
  = MealyFSM { mealyInitialState :: st, mealyInputs :: [it], mealyTransition :: Map (st, it) st, mealyEmit :: st -> it -> ot }

instance Functor (MealyFSM st it) where
  fmap f s@MealyFSM { mealyEmit=me } = s { mealyEmit = (f .) . me }

