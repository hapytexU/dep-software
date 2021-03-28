module Dep.Bricks.Layout where

data CircuitLayout
 = Horizontal
 | Vertical
 deriving (Bounded, Enum, Eq, Ord, Read, Show)
