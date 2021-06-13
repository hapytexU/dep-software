{-|
Module      : Dep.Bricks.Karnaugh
Description : A module to define three-value logic.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render, update, and interact with Karnaugh cards.
-}

module Dep.Bricks.Karnaugh (
    renderKarnaugh
  ) where

import Dep.Bricks.Utils(inRaster)
import Dep.Class.Renderable(CharRenderable)
import Dep.Data.Three(Three)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image)

-- | Render the given 'Three' as a /Karnaugh/ card, that can
-- also visualize the /sum-of-product/.
renderKarnaugh :: CharRenderable a
  => Three a  -- ^ The given 'Three' to render as a /Karnaugh card/.
  -> Attr  -- ^ The base 'Attr'ibute to render the /Karnaugh card/.
  -> Image  -- ^ The image that contains a rendered version of the /Karnaugh card/.
renderKarnaugh _ _ = inRaster undefined undefined
