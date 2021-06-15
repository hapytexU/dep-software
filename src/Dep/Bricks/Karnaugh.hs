{-# LANGUAGE BangPatterns #-}

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
import Dep.Class.Renderable(CharRenderable(charRenderItem))
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split), depth, leftmost)
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr, defAttr)
import Graphics.Vty.Image(Image, (<->), emptyImage, string)

type KLine = String
type KRaster = [KLine]

mergeVertical :: KLine -> Operator KRaster
mergeVertical _ ([], []) = []
mergeVertical zs (xs@(x:_), []) = xs ++ [zs']
  where zs' = zipWith const (cycle zs) x
mergeVertical zs (xs, ys@(y:_)) = xs ++ zs' : reverse ys
  where zs' = zipWith const (cycle zs) y

mergeHorizontal :: KLine -> Operator KRaster
mergeHorizontal spl = uncurry (zipWith3 f (cycle spl))
  where f sp xs ys = xs ++ sp : reverse ys

valueRaster :: [String]
valueRaster = undefined

{-_recurseDraw :: Int -> Operator Image -> Int
_recurseDraw _ _ = 0-}

recurse :: CharRenderable a => Operator KRaster -> Operator KRaster -> Int -> Three a -> KRaster
recurse ma mb !n = go
      where fn = recurse mb ma (n-1)
            go l |
              n <= 0 = [[charRenderItem (leftmost l)]]  -- leftmost is used to prevent cases with multiple items
            go l@(Leaf _) = let fnl = fn l in ma (fnl, fnl)
            go (Link l) = let fnl = fn l in ma (fnl, fnl)
            go (Split la lb) = ma (fn la, fn lb)

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
renderKarnaugh ts sop atr = inRaster atr (foldr ((<->) . string atr) emptyImage recs)
  where recs = recurse (mergeHorizontal "\x2502\x253c") (mergeVertical "\x2500\x253c") (depth ts) ts
