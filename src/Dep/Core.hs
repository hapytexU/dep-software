module Dep.Core (
    BFunc, BFunc1, BFunc2, BFunc3, BFunc4
  , Mergeable(merge)
  , WithDontCare(DontCare, Value)
  ) where

type BFunc = [Bool] -> Bool
type BoolFunc a = Bool -> a
type BFunc1 = BoolFunc Bool
type BFunc2 = BoolFunc BFunc1
type BFunc3 = BoolFunc BFunc2
type BFunc4 = BoolFunc BFunc3

data WithDontCare a
  = DontCare
  | Value a
  deriving (Eq, Ord, Read, Show)

instance Bounded a => Bounded (WithDontCare a) where
  minBound = DontCare
  maxBound = Value maxBound

class Mergeable a where
    merge :: a -> a -> Maybe a

instance Mergeable (Maybe a) where
    merge x@(Just _) Nothing = Just x
    merge Nothing x@(Just _) = Just x
    merge Nothing Nothing = Just Nothing
    merge (Just _) (Just _) = Nothing
