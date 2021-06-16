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
import Dep.Bricks.Utils(harrow', varrow', inRaster)
import Dep.Class.Renderable(CharRenderable(charRenderItem))
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split), depth, leftmost)
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, emptyImage, string)

type KLine = String
type KRaster = [KLine]

stem :: Attr -> Image
stem atr = string atr "\x2572 " <-> string atr " \x2572"

-- spacemark :: Attr -> Int -> Image -> Image
-- spacemark =

hmark :: (Image -> Image -> Image) -> String -> Attr -> Int -> Image
hmark f l atr n = f (string atr (replicate (div (n+3-length l) 2) ' ') <|> string atr l) (harrow' '\x2576' "\x2500" '\x2574' atr n)

hmark' :: String -> Attr -> Int -> Image
hmark' = hmark (<->)

hmark'' :: String -> Attr -> Int -> Image
hmark'' = hmark (flip (<->))

vmark :: (Image -> Image -> Image) -> String -> Attr -> Int -> Int -> Image
vmark _ _ atr m n = foldr ((<->) . char atr) (varrow' '\x2577' "\x2502" '\x2575' atr n) (replicate m ' ')

vmark' :: String -> Attr -> Int -> Int -> Image
vmark' = vmark (<|>)

vmark'' :: String -> Attr -> Int -> Int -> Image
vmark'' = vmark (flip (<|>))

_mergeVertical :: KLine -> Operator KRaster
_mergeVertical _ ([], []) = []
_mergeVertical zs (xs@(x:_), []) = xs ++ [zs']
  where zs' = zipWith const (cycle zs) x
_mergeVertical zs (xs, ys@(y:_)) = xs ++ zs' : reverse ys
  where zs' = zipWith const (cycle zs) y

_mergeHorizontal :: KLine -> Operator KRaster
_mergeHorizontal spl = uncurry (zipWith3 f (cycle spl))
  where f sp xs ys = xs ++ sp : reverse ys

_recurse :: CharRenderable a => Operator KRaster -> Operator KRaster -> Int -> Three a -> KRaster
_recurse ma mb !n = go
      where fn = _recurse mb ma (n-1)
            go l |
              n <= 0 = [[charRenderItem (leftmost l)]]  -- leftmost is used to prevent cases with multiple items
            go l@(Leaf _) = let fnl = fn l in ma (fnl, fnl)
            go (Link l) = let fnl = fn l in ma (fnl, fnl)
            go (Split la lb) = ma (fn la, fn lb)

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
renderKarnaugh ts _ atr = stem atr <|> (((string atr "     " <|> hmark' "x\x2081" atr 3) <-> (vmark' "x\x2082" atr 4 3 <|> inRaster atr (foldr ((<->) . string atr) emptyImage recs) <|> emptyImage ))) <-> (string atr "   " <|> hmark'' "x\x2083" atr 3)
  where recs = _recurse (_mergeHorizontal "\x2502\x253c") (_mergeVertical "\x2500\x253c") (depth ts) ts
