module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~), (^.))

import Dep.Bricks.Box(linev)
import Dep.Bricks.Gate(andGateH3)
import Dep.Bricks.Negation(negationHList)

import Graphics.Vty.Image((<|>), (<->), char)

circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  c <- getContext
  let a = c ^. attrL
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
  return (emptyResult & imageL .~ (char a ' ' <|> negationHList linev [False, True, True] a) <-> andGateH3 a)
