module Dep.Data.FiniteStateMachine where


data MooreFiniteStateMachine st it ot
  = MooreFiniteStateMachine { mooreStates :: [st], mooreInputs :: [it], mooreTransition :: st -> it -> Maybe st, mooreEmit :: st -> ot }
