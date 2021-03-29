module Dep.Bricks.Negation (
    negationList, negationHList, negationVList
  ) where

import Data.Bool(bool)

import Dep.Bricks.Box(negator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<|>), (<->), char, emptyImage)

negationList :: (Image -> Image -> Image) -> Char -> [Bool] -> Attr -> Image
negationList mgr lns bls attr = foldr (mgr . char attr . bool lns negator) emptyImage bls

negationHList :: Char -> [Bool] -> Attr -> Image
negationHList = negationList (<|>)

negationVList :: Char -> [Bool] -> Attr -> Image
negationVList = negationList (<->)
