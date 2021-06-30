{-# LANGUAGE BangPatterns, Safe #-}

{-|
Module      : Dep.Algorithm.Synthesis
Description : A module to convert a 'Three' of 'ThreeValue's into a sum-of-products of a product-of-sums.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines functions to generate a /sum-of-products/ or a /product-of-sums/ with the given
function specified by a 'Three'.
-}

module Dep.Algorithm.Synthesis (
    -- * Synthesize a 'Three'
    synthesis, synthesis'
  , synthesisSOP, synthesisSOP'
  , synthesisPOS, synthesisPOS'
    -- * Weigthed variants of the product and sum
  , WeightedProduct, WeightedSum
    -- * Create an upper and lowerbound Three
  , upperbound, lowerbound
    -- * Extract products and sums
  , extractProduct, extractSum
    -- * Processing a 'Three'
  , wipeout, wipeout'
  ) where

import Control.Applicative((<|>))

import Dep.Class.Opposite(opposite)
import Dep.Class.NonDeterministicWalkable(NonDeterministicWalkable(allNstep))
import Dep.Class.Simplify(simplify)
import Dep.Class.Walkable(Walkable(allStep, allWalkValues))
import Dep.Data.LogicItem(Item')
import Dep.Data.Product(Product(Product), Product', SumOfProducts(SumOfProducts), SumOfProducts')
import Dep.Data.Sum(ProductOfSums(ProductOfSums), ProductOfSums', Sum', Sum(Sum))
import Dep.Data.Three(Three(Leaf, Link, Split), depth, wipe)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One), fromBool, toLower, toUpper)

type WeightedItem = (Int, Item')

-- | A 2-tuple where the first item is the "weight" of the product, and the second one the corresponding 'Product''.
type WeightedProduct = (Int, Product')

-- | A 2-tuple where the first item is the "weight" of the sum, and the second one the corresponding 'Sum''.
type WeightedSum = (Int, Sum')

-- | Create a /simplified/ 'Three' where the 'DontCare' and 'One' map to 'True';
-- and 'Zero' maps to 'False'.
upperbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /upperbound/ from.
  -> Three Bool -- ^ The corresponding /upperbound/.
upperbound = simplify . fmap toUpper

-- | Create a /simplified/ 'Three' where the 'DontCare' and 'Zero' map to 'False';
-- and 'One' maps to 'True'.
lowerbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /lowerbound/ from.
  -> Three Bool -- ^ The corresponding /lowerbound/.
lowerbound = simplify . fmap toLower

_pushVal :: (Int -> Int) -> ThreeValue -> (Int, Product') -> (Int, Product')
_pushVal f x = go
    where go ~(!n, xs) = (f n, x:xs)

_pushVal' :: ThreeValue -> (Int, Product') -> (Int, Product')
_pushVal' = _pushVal succ

_pushVal'' :: (Int, Product') -> (Int, Product')
_pushVal'' = _pushVal id DontCare

extractItem
  :: ThreeValue  -- ^ The given 'ThreeValue' we are looking for. Typically this will be 'One' for a sum-of-products, and 'Zero' for a product of sums.
  -> Int  -- Y The maximum depth of the 'Three'.
  -> Three Bool -- ^ A 'Three' of 'Bool's that specifies if 'One' can be assigned to it (so either 'One' or 'DontCare').
  -> Three ThreeValue -- ^ A 'Three' of 'ThreeValue's in which we search for the given element.
  -> Maybe WeightedItem  -- ^ A path to a 'Leaf; with the given 'ThreeValue' together with the number of 'Zero's and 'One's in that path. If no path is found 'Nothing' is returned.
extractItem x' k _ = go k
  where go !n (Leaf x)
          | x == x' = Just (0, replicate n DontCare)
          | otherwise = Nothing
        go n (Link l) = _pushVal'' <$> go (n-1) l
        go n ~(Split la lb) = fmap (_pushVal' Zero) (go (n-1) la) <|> fmap (_pushVal' One) (go (n-1) lb)


-- | Obtain a 'Product'' together with the number of inputs for the AND gate if the 'Three'
-- contains at least one 'One'.
extractProduct
  :: Int  -- ^ The maximum depth of the 'Three'.
  -> Three Bool -- ^ A 'Three' of 'Bool's that specifies if 'One' can be assigned to it (so either 'One' or 'DontCare').
  -> Three ThreeValue -- ^ The 'Three' of 'ThreeValue's where we try to search for an item.
  -> Maybe WeightedProduct -- ^ A 2-tuple that contains the path to the leaf and the number of 'Zero's and 'One's in the path that measure the "weight" of the AND gate of the product.
extractProduct = extractItem One

-- | Obtain a 'Sum'' together with the number of inputs for the OR gates if the 'Three'
-- contains at least one 'Zero'.
extractSum
  :: Int  -- ^ The maximum depth of the 'Three'.
  -> Three Bool -- ^ A 'Three' of 'Bool's that specifies if 'Zero' can be assigned to it (so either 'Zero' or 'DontCare').
  -> Three ThreeValue -- ^ The 'Three' of 'ThreeValue's where we try to search for an item.
  -> Maybe WeightedSum -- ^ A 2-tuple that contains the path to the leaf and the number of 'Zero's and 'One's in the path that measure the "weight" of the OR gate of the product.
extractSum = extractItem Zero

-- | Convert the items that are accessed by the 'Product''
-- to a 'DontCare', and simplify the 'Three'. After wiping
-- out values, the 'Three' is simplified.
wipeout
  :: Product' -- ^ The product that specifies the path of the element(s) to set to 'DontCare'.
  -> Three ThreeValue  -- ^ The original 'Three' of 'ThreeValue's where we want to convert parts to 'DontCare'.
  -> Three ThreeValue  -- ^ The resulting 'Three' of 'ThreeValue's where items that match the path are wiped out.
wipeout path = simplify . wipeout' path

-- | Convert the items that are accessed by the 'Product''
-- to a 'DontCare', and simplify the 'Three'.
wipeout'
  :: Product' -- ^ The product that specifies the path of the element(s) to set to 'DontCare'.
  -> Three ThreeValue  -- ^ The original 'Three' of 'ThreeValue's where we want to convert parts to 'DontCare'.
  -> Three ThreeValue  -- ^ The resulting 'Three' of 'ThreeValue's where items that match the path are wiped out.
wipeout' = wipe DontCare

mergeSide :: (Int -> Maybe WeightedItem) -> Int -> Maybe (Int, Product') -> Maybe WeightedItem
mergeSide f n = go
    where go Nothing = f n
          go j@(~(Just ~(k, _))) = f k <|> j

_minimizeItem' :: ([Bool] -> Bool) -> Int -> [Bool] -> [Three Bool] -> Maybe WeightedItem
_minimizeItem' _ n _ _ | n <= 0 = Nothing
_minimizeItem' _ _ [] _ = Just (0, [])
_minimizeItem' mg n ~(x:xs) thr
  | mg (allWalkValues stepdc xs) = mergeSide (\i -> pshVal <$> _minimizeItem' mg (i-1) xs (allStep thr x)) n (_pushVal'' <$> _minimizeItem' mg n xs stepdc)
  | otherwise = pshVal <$> _minimizeItem' mg (n-1) xs stepx
  where stepdc = allNstep thr DontCare
        stepx = allStep thr x
        pshVal = _pushVal' (fromBool x)

minimizeProduct' :: Int -> [Bool] -> [Three Bool] -> Maybe WeightedProduct
minimizeProduct' = _minimizeItem' and

minimizeSum' :: Int -> [Bool] -> [Three Bool] -> Maybe WeightedSum
minimizeSum' = _minimizeItem' (not . or)

minimizeProduct :: Int -> Product' -> Three Bool -> Product'
minimizeProduct wght prd thr = maybe prd snd (minimizeProduct' wght (map toUpper prd) [thr])

minimizeSum :: Int -> Sum' -> Three Bool -> Sum'
minimizeSum wght prd thr = maybe prd snd (minimizeSum' wght (map toUpper prd) [thr])

-- | Create a 'SumOfProducts' object based on the given 'Three' of 'ThreeValue's. This function acts
-- as an alias for the 'synthesisSOP' function.
synthesis
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> SumOfProducts  -- ^ The sum of products that work with the function defined in the 'Three'.
synthesis = SumOfProducts . map Product . synthesis'

-- | Create a 'SumOfProducts' object based on the given 'Three' of 'ThreeValue's.
synthesisSOP
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> SumOfProducts  -- ^ The sum of products that work with the function defined in the 'Three'.
synthesisSOP = SumOfProducts . map Product . synthesisSOP'

-- | Create a 'ProductOfSums' object based on the given 'Three' of 'ThreeValue's.
synthesisPOS
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> ProductOfSums  -- ^ The product of sums that work with the function defined in the 'Three'.
synthesisPOS = ProductOfSums . map Sum . synthesisPOS'

-- | Create a sum-of-products for the given function of 'ThreeValue'. This function is an alias of
-- the 'synthesisSOP' function.
synthesis'
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> SumOfProducts'  -- ^ The sum of products that work with the function defined in the 'Three'.
synthesis' = synthesisSOP'

-- | Create a sum-of-products for the given function of 'ThreeValue'.
synthesisSOP'
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> SumOfProducts'  -- ^ The sum of products that work with the function defined in the 'Three'.
synthesisSOP' = _genericSynthesis' upperbound extractProduct minimizeProduct id

-- | Create a sum-of-products for the given function of 'ThreeValue'.
synthesisPOS'
  :: Three ThreeValue  -- ^ The 'Three' of 'ThreeValue's for which we want to make a logical formula.
  -> ProductOfSums'  -- ^ The sum of products that work with the function defined in the 'Three'.
synthesisPOS' = _genericSynthesis' lowerbound extractSum minimizeSum opposite

_genericSynthesis'
  :: (Three ThreeValue -> Three Bool)
  -> (Int -> Three Bool -> Three ThreeValue -> Maybe WeightedItem)
  -> (Int -> Item' -> Three Bool -> Item')
  -> (Item' -> Item')
  -> Three ThreeValue
  -> ProductOfSums'
_genericSynthesis' toBound _extractItem minimize post th = _synthesis _simp
  where _bound = toBound _simp
        _n = depth _simp
        _simp = simplify th
        _takeItem = _extractItem _n _bound
        _synthesis thr
          | Just (~(k, j)) <- _takeItem thr = let j' = minimize k j _bound in post j' : _synthesis (wipeout j' thr)
          | otherwise = []
