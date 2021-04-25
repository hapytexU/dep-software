{-# LANGUAGE OverloadedStrings, Safe #-}

module Dep.Data.Product (
    -- * Type synonyms to represent synthesis
    Product, Product', CompactProduct, CompactProduct', SumOfProducts, SumOfProducts'
    -- * Convert from compact to normal products
  , toCompact, fromCompact
    -- * Print products and sums-of-products
  , showSumOfProducts, showProduct, showProduct'
  ) where

import Data.Char.Small(asSub')
import Data.Text(Text, cons)

import Dep.Data.Three(ThreePath)
import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare))

type Product' = ThreePath
newtype Product = Product Product' deriving (Eq, Ord, Read, Show)

type CompactProduct' = [Int]
newtype CompactProduct = CompactProduct [Int] deriving (Eq, Ord, Read, Show)

type SumOfProducts' = [Product']
newtype SumOfProducts = SumOfProducts [Product] deriving (Eq, Ord, Read, Show)

toCompact :: Product' -> CompactProduct'
toCompact = go 1
    where go _ [] = []
          go n (One:xs) = n : go (n+1) xs
          go n (Zero:xs) = (-n) : go (n+1) xs
          go n (DontCare:xs) = go (n+1) xs

fromCompact :: CompactProduct' -> Product'
fromCompact = go 1
  where go _ [] = []
        go n ~(x:xs)
          | x == n = One : tl
          | x == -n = Zero : tl
          | otherwise = DontCare : tl
          where tl = go (n+1) xs

showSumOfProducts :: Char -> SumOfProducts' -> Text
showSumOfProducts _ [] = mempty
showSumOfProducts ci (x:xs) = go x xs
  where go z [] = go' mempty z
        go z ~(y:ys) = go' (" + " <> go y ys) z
        go' = showProduct' ci

showProduct :: Char -> Product' -> Text
showProduct = (`showProduct'` mempty)

showProduct' :: Char -> Text -> Product' -> Text
showProduct' ci tl = go (0 :: Int)
    where go _ [] = tl
          go n (DontCare:xs) = go (n+1) xs
          go n (One:xs) = _printvar id n xs
          go n ~(Zero:xs) = _printvar (cons '\'') n xs
          _printvar f n xs = cons ci (asSub' n) <> f (go (n+1) xs)
