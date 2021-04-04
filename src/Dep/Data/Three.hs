{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Dep.Data.Three (
    Three(Leaf, Link, Split)
  , three, depth
  , walk, path
  , simplify
  ) where

import Data.Bool(bool)

import Test.QuickCheck(frequency)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

data Three a
  = Leaf a
  | Link (Three a)
  | Split (Three a) (Three a)
  deriving (Eq, Foldable, Functor, Ord, Read, Show)

_linkLeaf :: Three a -> Three a
_linkLeaf x@(Leaf _) = x
_linkLeaf ~(Link x) = x

instance Applicative Three where
  pure = Leaf
  (<*>) (Leaf f) = fmap f
  (<*>) (Link f) = go
      where go (Split la lb) = Split (f <*> la) (f <*> lb)
            go l = Link (f <*> _linkLeaf l)
  (<*>) ~(Split fa fb) = go
      where go (Split xa xb) = Split (fa <*> xa) (fb <*> xb)
            go l = Split (fa <*> x') (fb <*> x') where x' = _linkLeaf l

three :: (a -> b) -> (b -> b) -> (b -> b -> b) -> Three a -> b
three f g h = go
  where go (Leaf a) = f a
        go (Link l) = g (go l)
        go ~(Split la lb) = h (go la) (go lb)

depth :: Three a -> Int
depth = three (const 0) succ ((succ .) . max)

_simplifyLink :: Three a -> Three a
_simplifyLink l@(Leaf _) = l
_simplifyLink l = Link l

simplify :: Eq a => Three a -> Three a
simplify l@(Leaf _) = l
simplify (Link l) = _simplifyLink (simplify l)
simplify (Split la lb)
  | sa == sb = _simplifyLink sa
  | otherwise = Split sa sb
  where sa = simplify la
        sb = simplify lb

walk :: Three a -> Bool -> Three a
walk l@(Leaf _) = const l
walk (Link t) = const t
walk ~(Split la lb) = bool la lb

path :: Three a -> [Bool] -> Three a
path = foldl walk

instance Arbitrary1 Three where
    liftArbitrary arb = go
      where go = frequency [(5, Leaf <$> arb), (2, Link <$> go), (1, Split <$> go <*> go)]

instance Arbitrary a => Arbitrary (Three a) where
    arbitrary = arbitrary1
