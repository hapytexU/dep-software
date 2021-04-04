module Dep.Data.ThreeValue (
    ThreeValue(DontCare, Zero, One)
  , threeValue, toMaybeBool
  , ThreeValues
  ) where

import Dep.Core(Mergeable(merge))

data ThreeValue
  = DontCare
  | Zero
  | One
  deriving (Enum, Eq, Ord, Read, Show)

threeValue :: a -> a -> a -> ThreeValue -> a
threeValue d z o = go
  where go DontCare = d
        go Zero = z
        go ~One = o

instance Mergeable ThreeValue where
  merge DontCare x = Just x
  merge x DontCare = Just x
  merge Zero Zero = Just Zero
  merge One One = Just One
  merge _ _ = Nothing

toMaybeBool :: ThreeValue -> Maybe Bool
toMaybeBool = threeValue Nothing (Just False) (Just True)

type ThreeValues = [ThreeValue]
