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

import Data.List(transpose)

import Dep.Algorithm.Synthesis(synthesis)
import Dep.Bricks.Utils(fromRaster, harrow', varrow', inRaster')
import Dep.Class.Renderable(CharRenderable(charRenderItem))
import Dep.Data.Product(SumOfProducts)
import Dep.Data.Three(Three(Leaf, Link, Split), depth, leftmost)
import Dep.Data.ThreeValue(ThreeValue)
import Dep.Utils(Operator)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, emptyImage, safeWcswidth, string)

type KLine = String
type KRaster = [KLine]

mapFrameH :: Char -> Char
mapFrameH '\x250f' = '\x2517'
mapFrameH '\x2517' = '\x250f'
mapFrameH '\x2513' = '\x251b'
mapFrameH '\x251b' = '\x2513'
mapFrameH '\x252f' = '\x2537'
mapFrameH '\x2537' = '\x252f'
mapFrameH c = c

mapFrameV :: Char -> Char
mapFrameV '\x250f' = '\x2513'
mapFrameV '\x2513' = '\x250f'
mapFrameV '\x2517' = '\x251b'
mapFrameV '\x251b' = '\x2517'
mapFrameV '\x2520' = '\x2528'
mapFrameV '\x2528' = '\x2520'
mapFrameV c = c

hmark :: Int -> Int -> String
hmark m n = replicate m ' ' ++ '\x2576' : replicate n '\x2500' ++ "\x2574"

hname :: Int -> Int -> String -> String
hname m n st = replicate (m + div (n-l+1) 2) ' ' ++ st
  where l = safeWcswidth st

twig :: Attr -> Image
twig atr = string atr "\x2572 " <-> string atr " \x2572"

addTopMark :: String -> KRaster -> KRaster
addTopMark st = (:) (hname 7 3 st) . (:) (hmark 7 3)

addBottomMark :: String -> KRaster -> KRaster
addBottomMark st = (++ [hmark 5 3, hname 5 3 st])

addRightMark :: String -> KRaster -> KRaster
addRightMark st kr = la ++ (l1 ++ "\x2577") : map (++"\x2502") lb ++ (l2 ++ '\x2502' : st) : map (++ "\x2502") lc ++ (l3 ++ "\x2575") : ld
  where ~(la, (l1:ls)) = splitAt 2 kr
        ~(lb, (l2:lt)) = splitAt 1 ls
        ~(lc, (l3:ld)) = splitAt 1 lt

addLeftMark :: String -> KRaster -> KRaster
addLeftMark st kr = map (spac1 ++) la ++ (spac ++ '\x2577' : l1) : map ((spac ++ "\x2502") ++) lb ++ (st ++ '\x2502' : l2) : map ((spac ++ "\x2502")++) lc ++ (spac ++ "\x2575" ++ l3) : map (spac1 ++) ld
  where ~(la, (l1:ls)) = splitAt 4 kr
        ~(lb, (l2:lt)) = splitAt 1 ls
        ~(lc, (l3:ld)) = splitAt 1 lt
        w = safeWcswidth st
        spac = replicate w ' '
        spac1 = ' ' : spac


{-
hmark :: (Image -> Image -> Image) -> String -> Attr -> Int -> Image
hmark f l atr n = f (string atr (replicate (div (n+3-length l) 2) ' ') <|> string atr l) (harrow' '\x2576' "\x2500" '\x2574' atr n)

hmark' :: String -> Attr -> Int -> Image
hmark' = hmark (<->)

hmark'' :: String -> Attr -> Int -> Image
hmark'' = hmark (flip (<->))
-}

vmark :: (Image -> Image -> Image) -> String -> Attr -> Int -> Int -> Image
vmark _ _ atr m n = foldr ((<->) . char atr) (varrow' '\x2577' "\x2502" '\x2575' atr n) (replicate m ' ')

vmark' :: String -> Attr -> Int -> Int -> Image
vmark' = vmark (<|>)

vmark'' :: String -> Attr -> Int -> Int -> Image
vmark'' = vmark (flip (<|>))

_mergeVertical :: KLine -> KRaster -> Int -> Operator KRaster
_mergeVertical spt spb n
  | n <= 4 = go
  | otherwise = go'
  where cyspt = zipWith const (cycle spt)
        cyspb = transpose . zipWith const (cycle spb)
        go ([], []) = []
        go (xs@(x:_), []) = xs ++ [cyspt x]
        go (xs, ys@(y:_)) = xs ++ cyspt y : reverse ys
        go' ([], []) = []
        go' (xs@(x:_), []) = xs ++ cyspb x
        go' (xs, ys@(y:_)) = xs ++ cyspb y ++ map (map mapFrameH) (reverse ys)


_mergeHorizontal :: KLine -> KRaster -> Int -> Operator KRaster
_mergeHorizontal spl spr n
  | n <= 4 = (uncurry (zipWith3 f (cycle spl)))
  | otherwise = (uncurry (zipWith3 f' (cycle spr)))
  where f sp xs ys = xs ++ sp : reverse ys
        f' sp xs ys = xs ++ sp ++ map mapFrameV (reverse ys)

_recurse :: CharRenderable a => (Int -> Operator KRaster) -> (Int -> Operator KRaster) -> Int -> Three a -> KRaster
_recurse ma mb !n = go
      where fn = _recurse mb ma (n-1)
            man = ma n
            go l |
              n <= 0 = [[charRenderItem (leftmost l)]]  -- leftmost is used to prevent cases with multiple items
            go l@(Leaf _) = let fnl = fn l in man (fnl, fnl)
            go (Link l) = let fnl = fn l in man (fnl, fnl)
            go ~(Split la lb) = man (fn la, fn lb)

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
renderKarnaugh ts _ atr = (fromRaster atr (inRaster' recs))
  -- ts _ atr = foldr ((<->) . string atr) emptyImage (addBottomMark "x\x2083" (addTopMark "x\x2081" (addLeftMark "x\x2082" (addRightMark "x\x2084" (inRaster' recs)))))
  where recs = _recurse (_mergeHorizontal "\x2502\x253c" ["\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x2528 \x2520", "\x2503 \x2503", "\x251b \x2517", "   ", "\x2513 \x250f"]) (_mergeVertical "\x2500\x253c" ["\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x2537 \x252f", "\x2501 \x2501", "\x251b \x2513", "   ", "\x2517 \x250f"] ) (depth ts) ts

-- TODO: mechanism for reverse

-- ["\x2501 \x2501", "\x2537 \x252f"]
