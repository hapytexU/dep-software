{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Dep.Data.Three (
    Three(Leaf, Link, Split)
  , three, depth
  , step, walk
  , simplify
  ) where

import Data.Bool(bool)

import Test.QuickCheck(frequency)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

-- | A data structure used to specify a mapping from a list of booleans
-- to a value in a more compact way. This datastructure can effectively
-- be used to define a sum of products or a product of sums.
data Three a
  = Leaf a  -- ^ A /leaf/ that contains a single value.
  | Link (Three a)  -- ^ A /link/ where it means that this variable does not matter but the next one(s) will.
  | Split (Three a) (Three a)  -- ^ A /split/ where this variable determines the outcome.
  deriving (Eq, Foldable, Functor, Ord, Read, Show)

_linkLeaf :: Three a -> Three a
_linkLeaf (Link x) = x
_linkLeaf x = x

-- | A catamorphism on the 'Three' object. Here one can provide functions
-- to use for the 'Leaf', the 'Link' and the 'Split' to catamorph a 'Three'
-- object to another object.
three :: (a -> b) -> (b -> b) -> (b -> b -> b) -> Three a -> b
three f g h = go
  where go (Leaf a) = f a
        go (Link l) = g (go l)
        go ~(Split la lb) = h (go la) (go lb)

-- | Construct a 'Three' that will apply the given function
-- for the items that satisfy the given /path/ of three-valued
-- objects.
apply
  :: (a -> a)  -- ^ The given function to apply for the items that satisfy the given path.
  -> [Maybe Bool]  -- ^ The given path to use.
  -> Three (a -> a)  -- ^ A 'Three' object of functions where the elements that satisfy
                     -- the path will use the given function and the others will use 'id'.
apply = foldr go . Leaf
  where go (Just False) = (`Split` lid)
        go (Just True) = Split lid
        go ~Nothing = Link
        lid = Leaf id

-- | Determine the maximum depth of the 'Three' tree.
depth
  :: Three a  -- ^ The 'Three' object to determine the maximum depth from.
  -> Int  -- ^ The depth of the given 'Three' object.
depth = three (const 0) succ ((succ .) . max)

_simplifyLink :: Three a -> Three a
_simplifyLink l@(Leaf _) = l
_simplifyLink l = Link l

-- | Simplify the given 'Three' object by minimizing common
-- subtrees. This is used to determine sum-of-products and
-- products-of-sums more efficiently, but can also be used
-- to make a table more readable.
simplify :: Eq a
  => Three a  -- ^ The given 'Three' to simplify.
  -> Three a  -- ^ The corresponding simplified 'Three'.
simplify l@(Leaf _) = l
simplify (Link l) = _simplifyLink (simplify l)
simplify (Split la lb)
  | sa == sb = _simplifyLink sa
  | otherwise = Split sa sb
  where sa = simplify la
        sb = simplify lb

-- | Take one step with the given 'Bool' that determines whether
-- to take the left or the right subtree.
step
  :: Three a  -- ^ The 'Three' object where we take a single step.
  -> Bool  -- ^ A 'Bool' that determines if we take the left of right subthree.
  -> Three a  -- ^ The corresponding subthree. For a 'Leaf' this is the same three.
step l@(Leaf _) = const l
step (Link t) = const t
step ~(Split la lb) = bool la lb

-- | Take a sequence of steps with the given list of 'Bool's.
walk
  :: Three a  -- ^ The given 'Three' object where we make a walk.
  -> [Bool]  -- ^ A list of 'Bool's that determine for each step if we take the left or right subthree.
  -> Three a  -- ^ The corresponding subthree. For a 'Leaf' we will each time keep returning the same leaf.
walk = foldl step

instance Applicative Three where
  pure = Leaf
  (<*>) (Leaf f) = fmap f
  (<*>) (Link f) = go
      where go (Split la lb) = Split (f <*> la) (f <*> lb)
            go l = Link (f <*> _linkLeaf l)
  (<*>) ~(Split fa fb) = go
      where go (Split xa xb) = Split (fa <*> xa) (fb <*> xb)
            go l = Split (fa <*> x') (fb <*> x') where x' = _linkLeaf l

instance Arbitrary1 Three where
    liftArbitrary arb = go
      where go = frequency [(5, Leaf <$> arb), (2, Link <$> go), (1, Split <$> go <*> go)]

instance Arbitrary a => Arbitrary (Three a) where
    arbitrary = arbitrary1
