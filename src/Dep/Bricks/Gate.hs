{-|
Module      : Dep.Bricks.Gate
Description : A module to render different types of gates for the /bricks/ UI.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module has functions to create gates with an arbitrary number of inputs.
-}

module Dep.Bricks.Gate (
    -- * Create generic gates
    gateH, gateV, gate, genericGate
    -- * A collection of /and/ gates
  , andGate, andGateH, andGateH2, andGateH3, andGateV, andGateV2, andGateV3
    -- * A collection of /or/ gates
  , orGate,  orGateH,  orGateH2,  orGateH3,  orGateV,  orGateV2,  orGateV3
  ) where

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

-- | Create a gate in the horizontal direction with the given 'Char' that specifies what type of gate it is, and the number of input wires.
gateH
  :: Char  -- ^ The 'Char' that will be printed (repeatedly) on the gate to specify the type of gate.
  -> Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
gateH ci n attr = gateLineH boxlt boxhu boxrt n attr <-> gateLineH boxv ci boxv n attr <-> string attr (boxlb : replicate n2 boxh ++ boxhd : replicate (n-n2-1) boxh ++ [boxrb])
    where n2 = div n 2

-- | Create a gate in the vertical direction with the given 'Char' that specifies what type of gate it is, and the number of input wires.
gateV
  :: Char  -- ^ The 'Char' that will be printed (repeatedly) on the gate to specify the type of gate.
  -> Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
gateV ci n attr = gateLineV boxlt boxvl boxlb n attr <|> gateLineV boxh ci boxh n attr <|> vstring attr (boxrt : replicate n2 boxv ++ boxvr : replicate (n-n2-1) boxv ++ [boxrb])
    where n2 = div n 2


-- | Create a gate in the given 'CircuitLayout' with the given 'Char' that specifies what type of gate it is, and the number of input wires.
gate
  :: Char  -- ^ The 'Char' that will be printed (repeatedly) on the gate to specify the type of gate.
  -> CircuitLayout -- ^ The given 'CircuitLayout' that specifies in what direction the gate is written.
  -> Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
gate = flip go
  where go Horizontal = gateH
        go Vertical = gateV

negationOut :: Int -> (Image -> Image -> Image) -> (Bool -> Attr -> Image) -> Bool -> Attr -> Image
negationOut n mgr wr bl atr = foldr (flip mgr) emptyImage (wr bl atr : replicate (n+1) (char atr ' '))

-- | A function to create a gate with negators and the input and output.
genericGate
  :: Char  -- ^ The 'Char' that specifies /what/ type of gate it is. This 'Char'acter will be printed (repeatedly) on the gate.
  -> CircuitLayout  -- ^ The 'CircuitLayout' that specifies in what direction the gate is written.
  -> [Bool]  -- ^ Specifies what wires have negators at the input side.
  -> Bool  -- ^ Specifies if the output side has a negator.
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
genericGate ci ly ngi ng atr = go ly
  where go Horizontal = (char atr ' ' <|> negationHList ngi atr) <-> gateH ci n atr <-> negationOut n2 (<|>) negationH ng atr
        go Vertical = (char atr ' ' <-> negationVList ngi atr) <|> gateV ci n atr <|> negationOut n2 (<->) negationV ng atr
        n = length ngi
        n2 = div n 2

-- | A helper function to create /and/ gates in a more effective way.
andGate
  :: CircuitLayout -- ^ The given 'CircuitLayout' that specifies in what direction the gate is written.
  -> Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGate = gate '&'

-- | A helper function to create /horizontal/ and gates in a more effective way.
andGateH
  :: Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateH = gateH '&'

-- | A helper function to create a horizontal and gate with /two/ input wires.
andGateH2
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateH2 = andGateH 2

-- | A helper function to create a horizontal and gate with /three/ input wires.
andGateH3
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateH3 = andGateH 3

-- | A helper function to create /vertical/ and gates in a more effective way.
andGateV
  :: Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateV = gateV '&'

-- | A helper function to create a vertical and gate with /two/ input wires.
andGateV2
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateV2 = andGateV 2

-- | A helper function to create a vertical and gate with /three/ input wires.
andGateV3
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
andGateV3 = andGateV 3


-- | A helper function to create /or/ gates in a more effective way.
orGate
  :: CircuitLayout -- ^ The given 'CircuitLayout' that specifies in what direction the gate is written.
  -> Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGate = gate '|'

-- | A helper function to create /horizontal/ or gates in a more effective way.
orGateH
  :: Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateH = orGate Horizontal

-- | A helper function to create a horizontal or gate with /two/ input wires.
orGateH2
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateH2 = orGateH 2

-- | A helper function to create a horizontal or gate with /three/ input wires.
orGateH3
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateH3 = orGateH 3

-- | A helper function to create /vertical/ or gates in a more effective way.
orGateV
  :: Int  -- ^ The number of input wires (/fan-in/).
  -> Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateV = orGate Vertical

-- | A helper function to create a vertical or gate with /two/ input wires.
orGateV2
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateV2 = orGateV 2

-- | A helper function to create a vertical or gate with /three/ input wires.
orGateV3
  :: Attr  -- ^ The 'Attr' that specifies the style of the gate.
  -> Image  -- ^ The corresponding 'Image'.
orGateV3 = orGateV 3
