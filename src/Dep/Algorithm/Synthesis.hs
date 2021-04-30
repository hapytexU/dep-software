{-# LANGUAGE Safe #-}

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
    synthesis
    -- * Create an upper and lowerbound Three
  , upperbound, lowerbound
  ) where

import Control.Applicative((<|>))

import Dep.Core(NonDeterministicWalkable(allnstep))
import Dep.Data.Product(Product', SumOfProducts')
import Dep.Data.Three(Three(Leaf, Link, Split), applyTo, simplify)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One))

-- | Create a /simplified/ 'Three' where the 'DontCare' and 'One' map to 'True';
-- and 'Zero' maps to 'False'.
upperbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /upperbound/ from.
  -> Three Bool -- ^ The corresponding /upperbound/.
upperbound = simplify . fmap (Zero /=)

-- | Create a /simplified/ 'Three' where the 'DontCare' and 'Zero' map to 'False';
-- and 'One' maps to 'True'.
lowerbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /lowerbound/ from.
  -> Three Bool -- ^ The corresponding /lowerbound/.
lowerbound = simplify . fmap (One ==)

_pushZero :: Functor f => f Product' -> f Product'
_pushZero = fmap (Zero:)

_pushOne :: Functor f => f Product' -> f Product'
_pushOne = fmap (One:)

{-guessProduct :: Three Bool -> Three ThreeValue -> Maybe ThreePath
guessProduct (Leaf True) thr = [] <$ extractProduct thr
guessPro
guessProduct (Link l) s@(Split _ _) = extractProduct s
guessProduct ()-}

extractProduct :: Three Bool -> Three ThreeValue -> Maybe Product'
extractProduct _ = go
  where go (Leaf One) = Just []
        go (Leaf _) = Nothing
        go (Link l) = (DontCare :) <$> go l
        go ~(Split la lb) = _pushZero (go la) <|> _pushOne (go lb)

wipeout :: Product' -> Three ThreeValue -> Three ThreeValue
wipeout path = simplify . applyTo (const DontCare) path

_incStep :: ThreeValue -> Int -> Int
_incStep DontCare = id
_incStep _ = (1+)

minimizeProduct' :: Product' -> [Three Bool] -> (Int, Product')
minimizeProduct' [] _ = (0, [])
minimizeProduct' (DontCare:xs) ts = (DontCare:) <$> minimizeProduct' xs (allnstep ts DontCare)
minimizeProduct' (x:xs) ts = (x:) <$> minimizeProduct' xs (allnstep ts DontCare)
--  | True <- c

minimizeProduct :: Product' -> Three Bool -> Product'
minimizeProduct p = snd . minimizeProduct' p . pure

-- | Create a sum-of-products for the given function of 'ThreeValue'.
synthesis :: Three ThreeValue -> SumOfProducts'
synthesis th = _synthesis _simp
  where _upper = upperbound _simp
        _simp = simplify th
        _takeProduct = extractProduct _upper
        _synthesis thr
          | Just j <- _takeProduct thr = let j' = minimizeProduct j _upper in j' : _synthesis (wipeout j' thr)
          | otherwise = []
