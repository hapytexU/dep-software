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

import Data.Char.Small(asSub')
import Data.List(transpose, zipWith4)
import Data.Text(unpack)

import Dep.Algorithm.Synthesis(synthesis)
import Dep.Bricks.Utils((+++), fromRaster, inRaster')  -- harrow', varrow'
import Dep.Class.Renderable(CharRenderable(charRenderItem))
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split), depth, leftmost)
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, safeWcswidth, string)  -- , (<->), (<|>), char, safeWcswidth, string)

type KLine = String
type KRaster = [KLine]

hmask :: Char -> Char -> Char -> Int -> Int -> String
hmask c0 ci cn = go
  where go n m = replicate n ' ' ++ c0 : replicate m ci ++ [cn]

hmask' :: Int -> Int -> String
hmask' = hmask '\x251c' '\x2500' '\x2524'

hadd0 :: Int -> String -> Int -> Attr -> Image -> Image
hadd0 0 _ _ atr  = ((char atr ' ' <-> char atr ' ') <->)
hadd0 dpt n0 i atr = ((string atr (hvarI n0 dpt i) <-> string atr (hmaskI dpt i)) <->)

vadd1 :: Int -> String -> Int -> Attr -> Image -> Image
vadd1 dpt n1 i atr
  | dpt < 2 = (twig atr 2 0 <|>)
  | otherwise = ((twig atr 2 (w - 1) <-> go) <|>)
  where w = safeWcswidth n1
        go = let ~(di, dj) = depthToAlign (dpt-1) in foldMap (string atr) (vvar n1 di dj +++ vmask' di dj)

hadd2 :: Int -> String -> Int -> Attr -> Image -> Image
hadd2 dpt n2 i atr
  | dpt <= 2 = id
  | otherwise = (<-> (string atr (hmaskJ dpt i) <-> string atr (hvarJ n2 dpt i)))

vadd3 :: Int -> String -> Int -> Attr -> Image -> Image
vadd3 dpt n3 i atr
  | dpt <= 3 = id
  | otherwise = (<|> go)
  where go = let ~(di, dj) = depthToAlign' (dpt-1) in foldMap (string atr) (vmask' di dj +++ vvar n3 di dj)

depthToAlign :: Int -> (Int, Int)
depthToAlign n = go (div (n+1) 2)
  where go 1 = (2, 1)
        go 2 = (4, 3)
        go 3 = (10, 7)
        go n
          | n <= 0 = (0, 0)
          | otherwise = let ~(di, dj) = go (n-1) in (2*di, 2*dj + 3)

depthToAlign' :: Int -> (Int, Int)
depthToAlign' n = go (div (n+1) 2)
  where go 1 = (1, 1)
        go 2 = (2, 3)
        go 3 = (4, 9)
        go 4 = (10, 17)
        go n
          | n <= 0 = (0, 0)
          | otherwise = let ~(di, dj) = go (n-1) in (2*di, 2*dj + 3)


twig :: Attr -> Int -> Int -> Image
twig atr n ofs = foldMap go (take n [ofs .. ])
  where go n = string atr (replicate n ' ') <|> char atr '\x2572'

hvar :: String -> Int -> Int -> String
hvar st n m = replicate (n + div (m-length st+3) 2) ' ' ++ st

hvarI :: String -> Int -> Int -> String
hvarI s dpt _ = uncurry (hvar s) (depthToAlign dpt)

hvarJ :: String -> Int -> Int -> String
hvarJ s dpt _ = uncurry (hvar s) (depthToAlign' dpt)

vmask :: Char -> Char -> Char -> Int -> Int -> [String]
vmask c0 ci cn = go
  where go n m = replicate n " " ++ [c0] : replicate m [ci] ++ [[cn]]

vmask' :: Int -> Int -> [String]
vmask' = vmask '\x252c' '\x2502' '\x2534'

hmaskI :: Int -> Int -> String
hmaskI dpt _ = uncurry hmask' (depthToAlign dpt)

hmaskJ :: Int -> Int -> String
hmaskJ dpt _ = uncurry hmask' (depthToAlign' dpt)

vvar :: String -> Int -> Int -> [String]
vvar st n m = replicate h0 ws ++ [st] ++ repeat ws
  where ws = replicate (safeWcswidth st) ' '
        h0 = n + div (m + 1) 2

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


_mergeHorizontal :: KLine -> (Int -> KRaster) -> Int -> Operator KRaster
_mergeHorizontal spl spr n
  | n <= 4 = uncurry (zipWith3 f spl)
  | otherwise = uncurry (zipWith3 f' (spr 0))
  where f sp xs ys = xs ++ sp : reverse ys
        f' sp xs ys = xs ++ sp ++ mapFrameV (reverse ys)
        m = div 15 2 :: Int
        l = div m 2 :: Int
        sp i | i < -l || i > l = " "
             | otherwise = "\x2502"

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

_horizontalThick :: Int -> [String]
--_horizontalThick _ = cycle (transpose (l0 : l1 : l2 : []))
--   where l0 = "\x2503\x2528\x2503\x2528\x2503\x2528\x2503\x251b \x2513"
--         l1 = repeat ' '
--         l2 = "\x2503\x2520\x2503\x2520\x2503\x2520\x2503\x2517 \x250f"
_horizontalThick _ = cycle ["\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x251b \x2517", "   ", "\x2513 \x250f"]

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
renderKarnaugh ts _ ns atr = vadd1 dpt n1 0 atr (hadd0 dpt n0 0 atr (hadd2 dpt n2 0 atr (vadd3 dpt n3 0 atr (fromRaster atr (inRaster' recs)))))
  where dpt = depth ts
        recs = _recurse (_mergeHorizontal _horizontalThin _horizontalThick) (_mergeVertical _verticalThin _verticalThick) dpt ts
        ~(n0:n1:n2:n3:ns') = ns ++ map (('x' :) . unpack . asSub') [0 ..]
        mask2 = string atr (hvar "x\x2082" 4 9) <-> string atr (hmask' 4 9)
--         swapit | even dpt = id
--               | otherwise = id
