module Dep.Bricks.Gate where

import Dep.Bricks.Box(boxh, boxhu, boxhd, boxlt, boxrt, boxlb, boxrb, boxv, boxvl, boxvr)
import Dep.Bricks.Layout(CircuitLayout(Horizontal, Vertical))
import Dep.Bricks.Negation(negationH, negationV, negationHList, negationVList)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, emptyImage, string)

vstring :: Attr -> String -> Image
vstring atr = foldr ((<->) . char atr) emptyImage

gateLineH :: Char -> Char -> Char -> Int -> Attr -> Image
gateLineH c0 ci cn n = (`string` (c0 : replicate n ci ++ [cn]))

gateLineV :: Char -> Char -> Char -> Int -> Attr -> Image
gateLineV c0 ci cn n = (`vstring` (c0 : replicate n ci ++ [cn]))

gateH :: Char -> Int -> Attr -> Image
gateH ci n attr = gateLineH boxlt boxhu boxrt n attr <-> gateLineH boxv ci boxv n attr <-> string attr (boxlb : replicate n2 boxh ++ boxhd : replicate (n-n2-1) boxh ++ [boxrb])
    where n2 = div n 2

gateV :: Char -> Int -> Attr -> Image
gateV ci n attr = gateLineV boxlt boxvl boxlb n attr <|> gateLineV boxh ci boxh n attr <|> vstring attr (boxrt : replicate n2 boxv ++ boxvr : replicate (n-n2-1) boxv ++ [boxrb])
    where n2 = div n 2

gate :: Char -> CircuitLayout -> Int -> Attr -> Image
gate = flip go
  where go Horizontal = gateH
        go Vertical = gateV

negationOut :: Int -> (Image -> Image -> Image) -> (Bool -> Attr -> Image) -> Bool -> Attr -> Image
negationOut n mgr wr bl atr = foldr (flip mgr) emptyImage (wr bl atr : replicate (n+1) (char atr ' '))

genericGate :: Char -> CircuitLayout -> [Bool] -> Bool -> Attr -> Image
genericGate ci ly ngi ng atr = go ly
  where go Horizontal = (char atr ' ' <|> negationHList ngi atr) <-> gateH ci n atr <-> negationOut n2 (<|>) negationH ng atr
        go Vertical = (char atr ' ' <-> negationVList ngi atr) <|> gateV ci n atr <|> negationOut n2 (<->) negationV ng atr
        n = length ngi
        n2 = div n 2

andGate :: CircuitLayout -> Int -> Attr -> Image
andGate = gate '&'

andGateH :: Int -> Attr -> Image
andGateH = andGate Horizontal

andGateH2 :: Attr -> Image
andGateH2 = andGateH 2

andGateH3 :: Attr -> Image
andGateH3 = andGateH 3

andGateV :: Int -> Attr -> Image
andGateV = andGate Vertical

andGateV2 :: Attr -> Image
andGateV2 = andGateV 2

andGateV3 :: Attr -> Image
andGateV3 = andGateV 3


orGate :: CircuitLayout -> Int -> Attr -> Image
orGate = gate '|'

orGateH :: Int -> Attr -> Image
orGateH = orGate Horizontal

orGateH2 :: Attr -> Image
orGateH2 = orGateH 2

orGateH3 :: Attr -> Image
orGateH3 = orGateH 3

orGateV :: Int -> Attr -> Image
orGateV = orGate Vertical

orGateV2 :: Attr -> Image
orGateV2 = orGateV 2

orGateV3 :: Attr -> Image
orGateV3 = orGateV 3
