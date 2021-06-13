{-# LANGUAGE TupleSections #-}

{-|
Module      : Dep.Bricks.Layered
Description : A module to combine several 'Image' layers to one 'Layer'.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module exports the 'mergeLayers' function that combine multiple 'Layer's in a single
layer. This is used if writing several layers is easier than writing a single image.
-}

module Dep.Bricks.Layered (
    -- * A type synoniem for one layer.
    Layer
    -- * Merge several layers to one layer.
  , createImage, mergeLayers
  ) where

import Dep.Utils(flatRaster)

import Graphics.Vty.Image(Image, (<->), (<|>), char, emptyImage)
import Graphics.Vty.Attributes(Attr)

-- | A raster is a list of lists of characters.
type Layer a = [[a]]

-- | Merge the given 'Layer's with the corresponding 'Attr'ibutes
-- to a single 'Image'.
mergeLayers
  :: (Char -> Bool)  -- ^ The predicate that determines if the upper layer will override the layer directly below.
  -> [(Attr, Layer Char)]  -- ^ The list of 'Layer's, the first item is the top item and the last item is the bottom layer.
  -> Image  -- ^ An image that we create by merging the different layers together.
mergeLayers p = createImage . flatRaster p

-- | Convert a list of lists of 2-tuples that contain an 'Attr'ibutes and a 'Char' to an 'Image'.
createImage
  :: [[(Attr, Char)]]  -- ^ The given list of lists of 'Char's
  -> Image
createImage = foldr ((<->) . (foldr ((<|>) . uncurry char) emptyImage)) emptyImage
