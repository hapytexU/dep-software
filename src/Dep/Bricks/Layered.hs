module Dep.Bricks.Layered where

import Graphics.Vty.Image(Image)
import Graphics.Vty.Attributes(Attr)

type Raster = [String]

mergeLayers :: [(Attr, Raster)] -> Image
mergeLayers = undefined
