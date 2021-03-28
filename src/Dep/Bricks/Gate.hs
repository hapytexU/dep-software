module Dep.Bricks.Gate where

import Dep.Bricks.Box(boxh, boxhu, boxhd, boxlt, boxrt, boxlb, boxrb, boxv, boxvl, boxvr)
import Dep.Bricks.Layout(CircuitLayout(Horizontal, Vertical))

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, emptyImage, string)

vstring :: Attr -> String -> Image
vstring atr = foldr ((<->) . char atr) emptyImage

gateLineH :: Char -> Char -> Char -> Int -> Attr -> Image
gateLineH c0 ci cn n = (`string` (c0 : replicate n ci ++ [cn]))

gateLineV :: Char -> Char -> Char -> Int -> Attr -> Image
gateLineV c0 ci cn n = (`vstring` (c0 : replicate n ci ++ [cn]))

gateH :: Char -> Int -> Attr -> Image
gateH ci n attr = gateLineH boxlt boxhu boxrt n attr <-> gateLineH boxv ci boxv n attr <-> string attr (boxlb : replicate n2 boxh ++ boxhd : replicate (n-n2) boxh ++ [boxrb])
    where n2 = div n 2

gateV :: Char -> Int -> Attr -> Image
gateV ci n attr = gateLineV boxlt boxvl boxlb n attr <|> gateLineV boxh ci boxh n attr <|> vstring attr (boxrt : replicate n2 boxv ++ boxvr : replicate (n-n2) boxv ++ [boxrb])
    where n2 = div n 2

gate :: Char -> CircuitLayout -> Int -> Attr -> Image
gate gt = (`go` gt)
  where go Horizontal = gateH
        go Vertical = gateV
