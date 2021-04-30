{-# LANGUAGE Safe #-}

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

upperbound :: Three ThreeValue -> Three Bool
upperbound = simplify . fmap (Zero /=)

lowerbound :: Three ThreeValue -> Three Bool
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
