module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~), (^.))

import Dep.Bricks.Box(linev)
import Dep.Bricks.Gate(andGateV3, negationList)

import Graphics.Vty.Image((<|>))

circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  c <- getContext
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
  return (emptyResult & imageL .~ negationList (<|>) linev [True, True, True, False, False, True, False, True, False, False, True] (c ^. attrL))
