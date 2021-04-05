{-|
Module      : Dep.Bricks.Circuit
Description : A module that exports widgets to render circuits effectively.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render circuits in an effective manner.
-}

module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~), (^.))

import Dep.Bricks.Gate(genericGate)
import Dep.Bricks.Layout(CircuitLayout(Horizontal))

-- | A widget that is used to render electronics circuitry.
circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  c <- getContext
  let a = c ^. attrL
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
  return (emptyResult & imageL .~ genericGate '&' Horizontal [False, True, False, True] True a)
