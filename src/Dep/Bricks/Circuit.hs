module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~), (^.))

import Dep.Bricks.Gate(andGateH2)

circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  c <- getContext
  return (emptyResult & imageL .~ andGateH2 (c ^. attrL))
