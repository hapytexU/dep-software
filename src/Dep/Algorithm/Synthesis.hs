module Dep.Algorithm.Synthesis where

import Control.Applicative((<|>))

import Dep.Data.Three(Three(Leaf, Link, Split), ThreePath, applyTo, simplify)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One))

upper :: ThreeValue -> Bool
upper Zero = False
upper _ = True

upperbound :: Three ThreeValue -> Three Bool
upperbound = simplify . fmap upper

_pushFalse :: Functor f => f [Maybe Bool] -> f [Maybe Bool]
_pushFalse = fmap (Just False:)

_pushTrue :: Functor f => f [Maybe Bool] -> f [Maybe Bool]
_pushTrue = fmap (Just True:)

{-guessProduct :: Three Bool -> Three ThreeValue -> Maybe ThreePath
guessProduct (Leaf True) thr = [] <$ extractProduct thr
guessPro
guessProduct (Link l) s@(Split _ _) = extractProduct s
guessProduct ()-}

extractProduct :: Three ThreeValue -> Maybe ThreePath
extractProduct (Leaf One) = Just []
extractProduct (Leaf _) = Nothing
extractProduct (Link l) = (Nothing :) <$> extractProduct l
extractProduct ~(Split la lb) = _pushFalse (extractProduct la) <|> _pushTrue (extractProduct lb)

wipeout :: ThreePath -> Three ThreeValue -> Three ThreeValue
wipeout path = simplify . applyTo (const DontCare) path

minimizeProduct :: ThreePath -> Three Bool -> [(Int, ThreePath)]
minimizeProduct = undefined

synthesis :: Three ThreeValue -> [ThreePath]
synthesis th = _synthesis (simplify th)
  where _upper = upperbound th
        _synthesis :: Three ThreeValue -> [ThreePath]
        _synthesis thr
          | Just j <- extractProduct thr = j : _synthesis (wipeout j thr)
          | otherwise = []
