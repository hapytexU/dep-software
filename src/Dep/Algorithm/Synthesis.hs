{-# LANGUAGE OverloadedStrings #-}

module Dep.Algorithm.Synthesis where

import Control.Applicative((<|>))

import Data.Char.Small(asSub')
import Data.Text(Text, cons)

import Dep.Data.Three(Three(Leaf, Link, Split), ThreePath, applyTo, simplify)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One))

type Product = ThreePath
type SumOfProducts = [Product]

upper :: ThreeValue -> Bool
upper Zero = False
upper _ = True

upperbound :: Three ThreeValue -> Three Bool
upperbound = simplify . fmap upper

_pushFalse :: Functor f => f Product -> f Product
_pushFalse = fmap (Zero:)

_pushTrue :: Functor f => f Product -> f Product
_pushTrue = fmap (One:)

{-guessProduct :: Three Bool -> Three ThreeValue -> Maybe ThreePath
guessProduct (Leaf True) thr = [] <$ extractProduct thr
guessPro
guessProduct (Link l) s@(Split _ _) = extractProduct s
guessProduct ()-}

extractProduct :: Three Bool -> Three ThreeValue -> Maybe Product
extractProduct _ = go
  where go (Leaf One) = Just []
        go (Leaf _) = Nothing
        go (Link l) = (DontCare :) <$> go l
        go ~(Split la lb) = _pushFalse (go la) <|> _pushTrue (go lb)

wipeout :: Product -> Three ThreeValue -> Three ThreeValue
wipeout path = simplify . applyTo (const DontCare) path

minimizeProduct :: Product -> Three Bool -> Product
minimizeProduct = const

synthesis :: Three ThreeValue -> SumOfProducts
synthesis th = _synthesis (simplify th)
  where _upper = upperbound th
        _takeProduct = extractProduct _upper
        _synthesis thr
          | Just j <- _takeProduct thr = let j' = minimizeProduct j _upper in j' : _synthesis (wipeout j' thr)
          | otherwise = []

showSumOfProducts :: Char -> SumOfProducts -> Text
showSumOfProducts _ [] = mempty
showSumOfProducts ci (x:xs) = go x xs
  where go z [] = go' mempty z
        go z ~(y:ys) = go' (" + " <> go y ys) z
        go' = showProduct' ci

showProduct :: Char -> Product -> Text
showProduct = (`showProduct'` mempty)

showProduct' :: Char -> Text -> Product -> Text
showProduct' ci tl = go (0 :: Int)
    where go _ [] = tl
          go n (DontCare:xs) = go (n+1) xs
          go n (One:xs) = _printvar id n xs
          go n ~(Zero:xs) = _printvar (cons '\'') n xs
          _printvar f n xs = cons ci (asSub' n) <> f (go (n+1) xs)
