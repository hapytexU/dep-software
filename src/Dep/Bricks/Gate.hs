module Dep.Bricks.Gate where

import Dep.Bricks.Layout(CircuitLayout(Horizontal, Vertical))

import Graphics.Vty.Image(Image, string)

gate :: Char -> CircuitLayout -> Int -> Image
gate gt = go
  where go Horizontal _ = string undefined ""
        go Vertical _ = string undefined ""
