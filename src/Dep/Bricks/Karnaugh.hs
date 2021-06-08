{-|
Module      : Dep.Bricks.Karnaugh
Description : A module to define three-value logic.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render, update, and interact with Karnaugh cards.
-}

module Dep.Bricks.Karnaugh where

import Dep.Bricks.Utils(inRaster)
import Dep.Class.Renderable(CharRenderable)
import Dep.Data.Three(Three)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image)

renderKarnaugh :: CharRenderable a => Three a -> Attr -> Image
renderKarnaugh _ _ = inRaster undefined undefined
