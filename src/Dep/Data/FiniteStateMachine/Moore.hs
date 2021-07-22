module Dep.Data.FiniteStateMachine.Moore where

import Data.HashMap(Map)

data MooreFSM st it ot
  = MooreFSM { mooreInitialState :: st, mooreInputs :: [it], mooreTransition :: Map (st, it) st, mooreEmit :: st -> ot }

-- getStates :: Eq st => MooreFSM st it ot
-- getStates FSM { mooreInitialState=st, mooreInputs=it, mooreTransition=mt } = go [] st
--   where go tl

instance Functor (MooreFSM st it) where
  fmap f s@MooreFSM { mooreEmit=me } = s { mooreEmit = f . me }

-- minimize :: Eq ot => MooreFiniteStateMachine st it ot -> MooreFiniteStateMachine st it ot
