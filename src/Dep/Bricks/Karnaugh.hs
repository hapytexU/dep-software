{-# LANGUAGE BangPatterns #-}

{-|
Module      : Dep.Bricks.Karnaugh
Description : A module to render Karnaugh cards in an interactive way.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render, update, and interact with Karnaugh cards.
-}

module Dep.Bricks.Karnaugh (
    renderKarnaugh, renderKarnaugh'
  , vvar
  ) where

import Data.List(transpose)

import Dep.Algorithm.Synthesis(synthesis)
import Dep.Bricks.Utils(fromRaster, inRaster')  -- harrow', varrow'
import Dep.Class.Renderable(CharRenderable(charRenderItem))
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split), depth, leftmost)
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), safeWcswidth, string)  -- , (<->), (<|>), char, safeWcswidth, string)

type KLine = String
type KRaster = [KLine]

-- markLeft :: [String]
-- markLeft =  +++

hmask :: Char -> Char -> Char -> Int -> Int -> String
hmask c0 ci cn = go
  where go n m = replicate n ' ' ++ c0 : replicate m ci ++ [cn]

hvar :: String -> Int -> Int -> String
hvar st n m = replicate (n + div (m-length st+1) 2) ' ' ++ st

vmask :: Char -> Char -> Char -> Int -> Int -> [String]
vmask c0 ci cn = go
  where go n m = replicate n " " ++ [c0] : replicate m [ci] ++ [[cn]]

vvar :: String -> Int -> Int -> [String]
vvar st n m = replicate h0 ws ++ [st] ++ repeat ws
  where ws = replicate (safeWcswidth st) ' '
        h0 = n + div (m + 1) 2

infixr 5 +++

(+++) :: [[a]] -> [[a]] -> [[a]]
(+++) = zipWith (++)


-- twig :: Attr -> Int -> Image
-- twig atr n = foldMap ((<|> char atr '\x2572') . string atr . (`replicate` ' ')) [0 .. n-1]

flipFrameH :: Char -> Char
flipFrameH '\x250f' = '\x2517'
flipFrameH '\x2517' = '\x250f'
flipFrameH '\x2513' = '\x251b'
flipFrameH '\x251b' = '\x2513'
flipFrameH '\x252f' = '\x2537'
flipFrameH '\x2537' = '\x252f'
flipFrameH c = c

flipFrameV :: Char -> Char
flipFrameV '\x250f' = '\x2513'
flipFrameV '\x2513' = '\x250f'
flipFrameV '\x2517' = '\x251b'
flipFrameV '\x251b' = '\x2517'
flipFrameV '\x2520' = '\x2528'
flipFrameV '\x2528' = '\x2520'
flipFrameV c = c

mapFrameH :: String -> String
mapFrameH = map flipFrameH

mapFrameV :: String -> String
mapFrameV = map flipFrameV

_mergeVertical :: KLine -> KRaster -> Int -> Operator KRaster
_mergeVertical spt spb n
  | n <= 4 = go
  | otherwise = go'
  where cyspt = zipWith const spt
        cyspb = transpose . zipWith const spb
        go ([], []) = []
        go (xs@(x:_), []) = xs ++ [cyspt x]
        go (xs, ys@(y:_)) = xs ++ cyspt y : reverse ys
        go' ([], []) = []
        go' (xs@(x:_), []) = xs ++ cyspb x
        go' (xs, ys@(y:_)) = xs ++ cyspb y ++ map mapFrameH (reverse ys)


_mergeHorizontal :: KLine -> KRaster -> Int -> Operator KRaster
_mergeHorizontal spl spr n
  | n <= 4 = uncurry (zipWith3 f spl)
  | otherwise = uncurry (zipWith3 f' spr)
  where f sp xs ys = xs ++ sp : reverse ys
        f' sp xs ys = xs ++ sp ++ mapFrameV (reverse ys)

_recurse :: CharRenderable a => (Int -> Operator KRaster) -> (Int -> Operator KRaster) -> Int -> Three a -> KRaster
_recurse ma mb !n = go
      where fn = _recurse mb ma (n-1)
            man = ma n
            go l
              | n <= 0 = [[charRenderItem (leftmost l)]]  -- leftmost is used to prevent cases with multiple items
            go l@(Leaf _) = let fnl = fn l in man (fnl, fnl)
            go (Link l) = let fnl = fn l in man (fnl, fnl)
            go ~(Split la lb) = man (fn la, fn lb)

_horizontalThin :: String
_horizontalThin = cycle "\x2502\x253c"

_horizontalThick :: [String]
_horizontalThick = cycle ["\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x251b \x2517", "   ", "\x2513 \x250f"]

_verticalThin :: String
_verticalThin = cycle "\x2500\x253c"

_verticalThick :: [String]
_verticalThick = cycle ["\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x251b \x2513", "   ", "\x2517 \x250f"]

-- | Render the given 'Three' as a /Karnaugh/ card, that can
-- also visualize the /sum-of-product/.
renderKarnaugh'
  :: Three ThreeValue  -- ^ The given 'Three' to render as a /Karnaugh card/.
  -> [String]  -- ^ The names of the variables that are rendered. If there are no sufficient variables, it will work with x₀, x₁, x₂, etc.
  -> Attr  -- ^ The base 'Attr'ibute to render the /Karnaugh card/.
  -> Image  -- ^ The image that contains a rendered version of the /Karnaugh card/.
renderKarnaugh' = renderKarnaugh <*> synthesis

-- | Render the given 'Three' as a /Karnaugh/ card, that can
-- also visualize the /sum-of-product/.
renderKarnaugh :: CharRenderable a
  => Three a  -- ^ The given 'Three' to render as a /Karnaugh card/.
  -> SumOfProducts -- ^ The sum of products that will be used to mark the /Karnaugh card/.
  -> [String]  -- ^ The names of the variables that are rendered. If there are no sufficient variables, it will work with x₀, x₁, x₂, etc.
  -> Attr  -- ^ The base 'Attr'ibute to render the /Karnaugh card/.
  -> Image  -- ^ The image that contains a rendered version of the /Karnaugh card/.
renderKarnaugh ts _ _ atr = string atr (hvar "x\x2080" 10 7) <-> string atr (hmask '\x251c' '\x2500' '\x2524' 10 7) <-> string atr (hvar "x\x2082" 4 9) <-> string atr (hmask '\x251c' '\x2500' '\x2524' 4 9) <-> fromRaster atr (inRaster' recs +++ (vmask '\x252c' '\x2502' '\x2534' 4 3 +++ vvar "x\x2081" 4 3))
-- renderKarnaugh ts _ _ atr = foldr ((<->) . string atr) emptyImage (addBottomMark "x\x2083" (addTopMark "x\x2081" (addLeftMark "x\x2082" (addRightMark "x\x2084" (inRaster' recs)))))
  where dpt = depth ts
        recs = swapit _recurse (_mergeHorizontal _horizontalThin _horizontalThick) (_mergeVertical _verticalThin _verticalThick) dpt ts
        swapit | even dpt = id
               | otherwise = id
