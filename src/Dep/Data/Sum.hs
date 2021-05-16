{-# LANGUAGE BangPatterns, MultiParamTypeClasses, OverloadedStrings, Safe #-}

{-|
Module      : Dep.Data.Sum
Description : A module that defines data structures for a sum, and a product-of-sums.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module provides utility functions to compress/decompress sums, and render these 'Product''s to
a unicode string.
-}

module Dep.Data.Sum (
    -- * Type synonyms to represent synthesis
    Sum, Sum', CompactSum, CompactSum', ProductOfSums, ProductOfSums'
    -- * Convert from compact to normal products
  , toCompact, fromCompact
    -- * Print sums and product of sums
  , showProductOfSums, showSum, showSum'
  ) where

import Data.Text(Text, cons)

import Dep.Data.LogicItem(EvaluateItem(evaluateItem), ToCompact(fromCompact, toCompact), getThreeList, putThreeList, subscriptVariable)
import Dep.Data.Three(ThreePath)
import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare))
import Data.Binary(Binary(get, put, putList))

-- | A type alias for a sum that is a 'ThreePath'.
type Sum' = ThreePath

-- | A data type that can be used to specify a sum. By using a newtype,
-- we can add special instance to the 'Sum'.
newtype Sum = Sum Sum' deriving (Eq, Ord, Read, Show)

instance EvaluateItem Sum where
  evaluateItem f ~(Sum s) = go 1 s
    where go _ [] = False
          go !n (Zero:xs) = not (f n) || go (n+1) xs
          go !n (One:xs) = f n || go (n+1) xs
          go !n (~DontCare:xs) = go (n+1) xs

-- | A more compact representation of a sum where the indexes that have 'Zero'
-- or 'One' are listed by the /positive/ or /negative/ index respectively.
type CompactSum' = [Int]

-- | A data type that can be used to specify a 'CompactSum'. By using a new
-- type, this means that we can define other instances than these for a list of 'Int'.
newtype CompactSum = CompactSum CompactSum' deriving (Eq, Ord, Read, Show)

instance EvaluateItem CompactSum where
  evaluateItem f ~(CompactSum s) = go s
    where go [] = False
          go (n:ns)
            | n < 0 = not (f (-n)) || go ns
            | otherwise = f n || go ns

-- | A type synonym to present a product of sums where each item of the list is a 'Sum''.
type ProductOfSums' = [Sum']

-- | A data type that is used to specify a product of sums. This type can be used
-- to specify new instance other than these of a list of lists of 'Int's.
newtype ProductOfSums = ProductOfSums [Sum] deriving (Eq, Ord, Read, Show)

instance EvaluateItem ProductOfSums where
  evaluateItem f ~(ProductOfSums p) = all (evaluateItem f) p


instance ToCompact Sum CompactSum where
  toCompact (Sum s) = CompactSum (toCompact s)
  fromCompact (CompactSum s) = Sum (fromCompact s)

-- | Convert the given product of sums to a 'Text' object that presents
-- the 'ProductOfSums'' as a 'Text' object with variables as subscript.
--
-- >>> showProductOfSums 'x' [[One, One], [DontCare, Zero, One]]
-- "(x₀x₁)(x₁'x₂)"
showProductOfSums
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> ProductOfSums' -- ^ The given sum of products to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'SumOfProducts''.
showProductOfSums = showProductOfSums' . subscriptVariable

-- | Convert the given product of sums to a 'Text' object that presents
-- the 'ProductOfSums'' as a 'Text' object with variables as subscript.
--
-- >>> showProductOfSums' (subscriptVariable 'y') [[One, One], [DontCare, Zero, One]]
-- "(y₀ + y₁)(y₁ + 'y₂)"
showProductOfSums'
  :: (Int -> Text) -- ^ A function that maps the given index to the variable name.
  -> ProductOfSums' -- ^ The given sum of products to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'SumOfProducts''.
showProductOfSums' _ [] = mempty
showProductOfSums' f (x:xs) = go x xs
  where go z [] = go' mempty z
        go z ~(y:ys) = go' (" + " <> go y ys) z
        go' = showSum'' f

-- | Print a given sum as a sequence of variables that can be negated.
-- for example:
--
-- >>> showSum 'x' [One, DontCare, Zero, One]
-- "x₀ + x₂' + x₃"
showSum
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> Sum' -- ^ The given sum to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Product''.
showSum = (`showSum'` mempty)

-- | Print a given sum as a sequence of variables that can be negated.
-- for example:
--
-- >>> showSum' 'x' mempty [One, DontCare, Zero, One]
-- "x₀ + x₂' + x₃"
showSum'
  :: Char  -- ^ The name of the root variable that will be used with subscripts.
  -> Text  -- ^ The text that will be added as tail, this is useful if we combine Sums.
  -> Sum' -- ^ The given sum to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Sum''.
showSum' = showSum'' . subscriptVariable

-- | Print a given sum as a sequence of variables that can be negated.
-- for example:
--
-- >>> showSum' (subscriptVariable 'y') mempty [One, DontCare, Zero, One]
-- "y₀ + y₂' + y₃"
showSum''
  :: (Int -> Text) -- ^ A function that maps the given index to the variable name.
  -> Text  -- ^ The text that will be added as tail, this is useful if we combine sums.
  -> Sum' -- ^ The given sum to convert to a 'Text'.
  -> Text -- ^ The corresponding 'Text' object that presents the given 'Sum''.
showSum'' ci tl = go (0 :: Int)
    where go _ [] = tl
          go n (DontCare:xs) = go (n+1) xs
          go n (One:xs) = _printvar id n xs
          go n ~(Zero:xs) = _printvar (cons '\'') n xs
          _printvar f n xs = ci n <> f (go (n+1) xs)

instance Binary Sum where
  get  = Sum <$> getThreeList
  put (Sum s) = putThreeList s

instance Binary ProductOfSums where
  get = ProductOfSums <$> get
  put (ProductOfSums ps) = putList ps