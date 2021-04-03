module Dep.Bricks.Negation (
    negation, negationList, negationHList, negationVList
  ) where

import Data.Bool(bool)

import Dep.Bricks.Box(negator, linev, lineh)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<|>), (<->), char, emptyImage)

negation :: Char -> Bool -> Attr -> Image
negation c = flip char . bool negator c

negationList :: (Image -> Image -> Image) -> Char -> [Bool] -> Attr -> Image
negationList mgr lns bls attr = foldr (mgr . (`go` attr)) emptyImage bls
  where go = negation lns

negationHList :: [Bool] -> Attr -> Image
negationHList = negationList (<|>) linev

negationVList :: [Bool] -> Attr -> Image
negationVList = negationList (<->) lineh
