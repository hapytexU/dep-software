{-|
Module      : Dep.Bricks.Karnaugh
Description : A module to define three-value logic.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render, update, and interact with Karnaugh cards.
-}

module Dep.Bricks.Karnaugh (
    renderKarnaugh, renderKarnaugh'
  ) where

import Dep.Algorithm.Synthesis(synthesis)
import Dep.Bricks.Utils(inRaster)
import Dep.Class.Renderable(CharRenderable)
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split))
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image)

type KRaster = [String]

mergeVertical :: KRaster -> KRaster -> KRaster
mergeVertical = (<>)

mergeHorizontal :: KRaster -> KRaster -> KRaster
mergeHorizontal = zipWith (<>)

valueRaster :: [String]
valueRaster = undefined

{-_recurseDraw :: Int -> Operator Image -> Int
_recurseDraw _ _ = 0-}

recurse :: CharRenderable a => Operator KRaster -> Operator KRaster -> Int -> Three a -> KRaster
recurse ma mb n = uncurry mergeVertical . go
      where fn n = recurse mb ma (n-1)
            go' 0 ~(Leaf l) = [[charRenderItem l]]
            go' _ l@(Leaf _) = let fnl = fn n l in (fnl, fnl)
            go' _ (Link l) = let fnl = fn n l in (fnl, fnl)
            go' _ ~(Split la lb) = (fn n la, fn n lb)

-- renderCard :: CharRenderable a => Three a -> String

-- | Render the given 'Three' as a /Karnaugh/ card, that can
-- also visualize the /sum-of-product/.
renderKarnaugh'
  :: Three ThreeValue  -- ^ The given 'Three' to render as a /Karnaugh card/.
  -> Attr  -- ^ The base 'Attr'ibute to render the /Karnaugh card/.
  -> Image  -- ^ The image that contains a rendered version of the /Karnaugh card/.
renderKarnaugh' = renderKarnaugh <*> synthesis

-- | Render the given 'Three' as a /Karnaugh/ card, that can
-- also visualize the /sum-of-product/.
renderKarnaugh :: CharRenderable a
  => Three a  -- ^ The given 'Three' to render as a /Karnaugh card/.
  -> SumOfProducts -- ^ The sum of products that will be used to mark the /Karnaugh card/.
  -> Attr  -- ^ The base 'Attr'ibute to render the /Karnaugh card/.
  -> Image  -- ^ The image that contains a rendered version of the /Karnaugh card/.
renderKarnaugh _ _ _ = inRaster undefined undefined
