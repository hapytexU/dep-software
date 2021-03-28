module Dep.Bricks.Gate where

import Dep.Bricks.Box(boxhu, boxlt, boxrt, boxv)
import Dep.Bricks.Layout(CircuitLayout(Horizontal, Vertical))

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), string)

gateLine :: Char -> Char -> Char -> Int -> Attr -> Image
gateLine c0 ci cn n = (`string` (c0 : replicate n ci ++ [cn]))

gateH :: Char -> Int -> Attr -> Image
gateH ci n attr = gateLine boxlt boxhu boxrt n attr <-> gateLine boxv ci boxv n attr

gateV :: Char -> Int -> Attr -> Image
gateV = undefined

gate :: Char -> CircuitLayout -> Int -> Attr -> Image
gate gt = (`go` gt)
  where go Horizontal = gateH
        go Vertical = gateV
