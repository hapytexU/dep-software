module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~), (^.))

import Dep.Bricks.Box(linev)
import Dep.Bricks.Gate(genericGate)
import Dep.Bricks.Layout(CircuitLayout(Horizontal))

import Graphics.Vty.Image((<|>), (<->), char)

circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  c <- getContext
  let a = c ^. attrL
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
  return (emptyResult & imageL .~ (genericGate '&' Horizontal [False, True, False, True] True a))
