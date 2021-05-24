{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses, OverloadedStrings, Safe, TypeApplications #-}

{-|
Module      : Dep.Data.Product
Description : A module that defines data structures for a product, and a sum-of-products.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module provides utility functions to compress/decompress products, and render these 'Product''s to
a unicode string.
-}

module Dep.Data.Product (
    -- * Type synonyms to represent synthesis
    Product(Product), Product', CompactProduct(CompactProduct), CompactProduct', SumOfProducts(SumOfProducts), SumOfProducts'
    -- * Print products and sums-of-products
  , showSumOfProducts, showProduct, showProduct', subscriptVariable
  ) where

import Data.Binary(Binary(get, put), putList)
import Data.Data(Data)
import Data.Hashable(Hashable)
import Data.Text(Text, cons)

import Dep.Data.LogicItem(EvaluateItem(evaluateItem), ToCompact(fromCompact, toCompact), getThreeList, putThreeList, subscriptVariable)
import Dep.Data.Three(ThreePath)
import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare))

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary, shrink))

-- | A type alias for a product that is a 'ThreePath'.
type Product' = ThreePath

-- | A data type that can be used to specify a product. By using a newtype,
-- we can add special instance to the 'Product'.
newtype Product = Product Product' deriving (Data, Eq, Generic, Ord, Read, Show)

instance Arbitrary Product where
  arbitrary = Product <$> arbitrary
  shrink (Product p) = Product <$> shrink p

instance Binary Product where
  get  = Product <$> getThreeList
  put (Product p) = putThreeList p

instance EvaluateItem Product where
  evaluateItem f ~(Product p) = go 1 p
    where go _ [] = True
          go !n (Zero:xs) = not (f n) && go (n+1) xs
          go !n (One:xs) = f n && go (n+1) xs
          go !n (~DontCare:xs) = go (n+1) xs

instance Hashable Product

instance ToCompact Product CompactProduct where
  toCompact (Product s) = CompactProduct (toCompact s)
  fromCompact (CompactProduct s) = Product (fromCompact s)

-- | A more compact representation of a product where the indexes that have 'Zero'
-- or 'One' are listed by the /positive/ or /negative/ index respectively.
type CompactProduct' = [Int]

-- | A data type that can be used to specify a 'CompactProduct'. By using a new
-- type, this means that we can define other instances than these for a list of 'Int'.
newtype CompactProduct = CompactProduct CompactProduct' deriving (Data, Eq, Generic, Ord, Read, Show)

instance Arbitrary CompactProduct where
  arbitrary = toCompact @Product <$> arbitrary
  shrink cp = toCompact @Product <$> shrink (fromCompact cp)

instance Binary CompactProduct where
  put (CompactProduct cp) = put cp
  get = CompactProduct <$> get

instance EvaluateItem CompactProduct where
  evaluateItem f ~(CompactProduct p) = all go p
    where go n
            | n < 0 = not (f (-n))
            | otherwise = f n

instance Hashable CompactProduct

-- | A type synonym to present a sum of products where each item of the list is a 'Product''.
type SumOfProducts' = [Product']

-- | A data type that is used to specify a sum of products. This type can be used
-- to specify new instance other than these of a list of lists of 'Int's.
newtype SumOfProducts = SumOfProducts [Product] deriving (Data, Eq, Generic, Ord, Read, Show)

instance Arbitrary SumOfProducts where
  arbitrary = SumOfProducts <$> arbitrary
  shrink (SumOfProducts sop) = SumOfProducts <$> shrink sop

instance Binary SumOfProducts where
  get = SumOfProducts <$> get
  put (SumOfProducts sp) = putList sp

instance EvaluateItem SumOfProducts where
  evaluateItem f ~(SumOfProducts s) = any (evaluateItem f) s

instance Hashable SumOfProducts

-- | Convert the given sum of products to a 'Text' object that presents
-- the 'SumOfProducts'' as a 'Text' object with variables as subscript.
--
-- >>> showSumOfProducts 'x' [[One, One], [DontCare, Zero, One]]
-- "x₀x₁ + x₁'x₂"
showSumOfProducts
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> SumOfProducts' -- ^ The given sum of products to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'SumOfProducts''.
showSumOfProducts = showSumOfProducts' . subscriptVariable

-- | Convert the given sum of products to a 'Text' object that presents
-- the 'SumOfProducts'' as a 'Text' object with variables as subscript.
--
-- >>> showSumOfProducts' (subscriptVariable 'y') [[One, One], [DontCare, Zero, One]]
-- "y₀y₁ + y₁'y₂"
showSumOfProducts'
  :: (Int -> Text) -- ^ A function that maps the given index to the variable name.
  -> SumOfProducts' -- ^ The given sum of products to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'SumOfProducts''.
showSumOfProducts' _ [] = mempty
showSumOfProducts' f (x:xs) = go x xs
  where go z [] = go' mempty z
        go z ~(y:ys) = go' (" + " <> go y ys) z
        go' = showProduct'' f

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
showProduct' = showProduct'' . subscriptVariable

-- | Print a given product as a sequence of variables that can be negated.
-- for example:
--
-- >>> showProduct' (subscriptVariable 'y') mempty [One, DontCare, Zero, One]
-- "y₀y₂'y₃"
showProduct''
  :: (Int -> Text) -- ^ A function that maps the given index to the variable name.
  -> Text  -- ^ The text that will be added as tail, this is useful if we combine products.
  -> Product' -- ^ The given product to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Product''.
showProduct'' ci tl = go (0 :: Int)
    where go _ [] = tl
          go n (DontCare:xs) = go (n+1) xs
          go n (One:xs) = _printvar id n xs
          go n ~(Zero:xs) = _printvar (cons '\'') n xs
          _printvar f n xs = ci n <> f (go (n+1) xs)
