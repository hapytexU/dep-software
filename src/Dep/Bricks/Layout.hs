{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe #-}

{-|
Module      : Dep.Bricks.Layout
Description : A module to specify how a circuit is rendered.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Specifies how a circuit is rendered. This can be in a /horizontal/ and /vertical/ manner.
-}

module Dep.Bricks.Layout (
    -- * The orientation of a circuit
    CircuitLayout(Horizontal, Vertical)
  ) where

import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

-- | A data type that specifies whether the circuit is render in a horizontal or vertical manner.
data CircuitLayout
  = Horizontal  -- ^ The /horizontal/ orientation.
  | Vertical  -- ^ The /vertical/ orientation.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable CircuitLayout
