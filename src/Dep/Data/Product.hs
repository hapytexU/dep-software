{-# LANGUAGE OverloadedStrings, Safe #-}

{-|
Module      : Dep.Data.Product
Description : A module that defines data structures for a product, and a sum-of-products.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This
-}

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

-- | A type alias for a product that is a 'ThreePath'.
type Product' = ThreePath

-- | A data type that can be used to specify a product. By using a newtype,
-- we can add special instance to the 'Product'.
newtype Product = Product Product' deriving (Eq, Ord, Read, Show)

-- | A more compact representation of a product where the indexes that have 'Zero'
-- or 'One' are listed by the /positive/ or /negative/ index respectively.
type CompactProduct' = [Int]

-- | A data type that can be used to specify a 'CompactProduct'. By using a new
-- type, this means that we can define other instances than these for a list of 'Int'.
newtype CompactProduct = CompactProduct CompactProduct' deriving (Eq, Ord, Read, Show)

-- | A type synonym to present a sum of products where each item of the list is a 'Product''.
type SumOfProducts' = [Product']

-- | A data type that is used to specify a sum of products. This type can be used
-- to specify new instance other than these of a list of lists of 'Int's.
newtype SumOfProducts = SumOfProducts [Product] deriving (Eq, Ord, Read, Show)

-- | Convert the given 'Product'' to a compact representation as a 'CompactProduct''.
toCompact
  :: Product'  -- ^ The given 'Product'' to convert.
  -> CompactProduct'  -- ^ The corresponding 'CompactProduct''.
toCompact = go 1
    where go _ [] = []
          go n (One:xs) = n : go (n+1) xs
          go n (Zero:xs) = (-n) : go (n+1) xs
          go n (DontCare:xs) = go (n+1) xs

-- | Convert the given 'CompactProduct'' to a corresponding 'Product''.
-- This function is not the complete inverse of 'toCompact', since 'DontCare's
-- at the end of the 'Product'' will not be rendered.
fromCompact
  :: CompactProduct'  -- ^ The given 'CompactProduct'' that is converted to a 'Product''.
  -> Product'  -- ^ A corresponding 'Product''.
fromCompact = go 1
  where go _ [] = []
        go n xa@(~(x:xs))
          | x == n = One : tl xs
          | x == -n = Zero : tl xs
          | otherwise = DontCare : tl xa
          where tl = go (n+1)

-- | Convert the given sum of products to a 'Text' object that presents
-- the 'SumOfProducts'' as a 'Text' object with variables as subscript.
--
-- >>> showSumOfProducts 'x' [[One, One], [DontCare, Zero, One]]
-- "x₀x₁ + x₁'x₂"
showSumOfProducts
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> SumOfProducts' -- ^ The given sum of products to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'SumOfProducts''.
showSumOfProducts _ [] = mempty
showSumOfProducts ci (x:xs) = go x xs
  where go z [] = go' mempty z
        go z ~(y:ys) = go' (" + " <> go y ys) z
        go' = showProduct' ci

-- | Print a given product as a sequence of variables that can be negated.
-- for example:
--
-- >>> showProduct 'x' [One, DontCare, Zero, One]
-- "x₀x₂'x₃"
showProduct
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> Product' -- ^ The given product to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Product''.
showProduct = (`showProduct'` mempty)

-- | Print a given product as a sequence of variables that can be negated.
-- for example:
--
-- >>> showProduct' 'x' mempty [One, DontCare, Zero, One]
-- "x₀x₂'x₃"
showProduct'
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> Text  -- ^ The text that will be added as tail, this is useful if we combine products.
  -> Product' -- ^ The given product to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Product''.
showProduct' ci tl = go (0 :: Int)
    where go _ [] = tl
          go n (DontCare:xs) = go (n+1) xs
          go n (One:xs) = _printvar id n xs
          go n ~(Zero:xs) = _printvar (cons '\'') n xs
          _printvar f n xs = cons ci (asSub' n) <> f (go (n+1) xs)
