{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

{-|
Module      : Dep.Data.ThreeValue
Description : A module that defines a /three/ data structure, used for lookup tables with don't cares.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This modules defines a /three/, a tree-like data structure with a leaf, a node, and a link where both
subtrees are the same. This is used to make more compact and efficient representations of a boolean function.
-}

module Dep.Data.Three (
    -- * Defining a three
    Three(Leaf, Link, Split)
    -- * Paths in a three
  , ThreePath
    -- * Catamorphisms
  , three, depth
    -- * Lookups and constructions
  , step, nstep', nstep, walk, apply, applyTo
    -- * Simplifying
  , simplify
    -- * Retrieve children according to a path
  , children, children'
  ) where

import Control.Applicative(Applicative(liftA2))

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

type ThreePath = [Maybe Bool]

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
  -> ThreePath  -- ^ The given path to use.
  -> Three (a -> a)  -- ^ A 'Three' object of functions where the elements that satisfy
                     -- the path will use the given function and the others will use 'id'.
apply = foldr go . Leaf
  where go (Just False) = (`Split` lid)
        go (Just True) = Split lid
        go ~Nothing = Link
        lid = Leaf id

_getChildren :: Three a -> (Three a, Three a)
_getChildren l@(Leaf _) = (l, l)
_getChildren (Link l) = (l, l)
_getChildren (Split la lb) = (la, lb)

-- | Apply the given function to the elements in the given 'Three' that satisfy the given path.
applyTo
  :: (a -> a)  -- ^ The given function to apply to some parts of the 'Three'.
  -> ThreePath  -- ^ The given path that specifies what for what parts of the 'Three' we should apply the function.
  -> Three a  -- ^ The given 'Three' where (part) of the 'Three' will be modified with a given function.
  -> Three a  -- ^ The resulting 'Three' after applying the given function to parts of the 'Three' that satisfy the given path.
applyTo f = go
  where go [] = fmap f
        go (Nothing:xs) = go'
          where go' (Split la lb) = Split (go xs la) (go xs lb)
                go' l@(Leaf _) = Link (go xs l)
                go' (Link l) = Link (go xs l)
        go ~(~(Just sl):xs) = go' sl . _getChildren
          where go' True ~(la, lb) = Split la (go xs lb)
                go' ~False ~(la, lb) = Split (go xs la) lb

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

-- | Take a non-deterministic step where a 'Nothing' means we work with both 'True'
-- and 'False'.
nstep'
  :: Three a  -- ^ The given 'Three' where we make a non-determinstic step.
  -> Maybe Bool  -- ^ The step that we make, this can be 'Nothing' if we want to query both 'True' and 'False'.
  -> [Three a]  -- ^ The list of tail elements added to the result.
  -> [Three a]  -- ^ The list of the possible 'Three's with the step.
nstep' l@(Leaf _) = const (l:)
nstep' (Link t) = const (t:)
nstep' (Split la lb) = go
    where go (Just False) = (la:)
          go (Just True) = (lb:)
          go ~Nothing = (la:) . (lb:)

-- | Take a non-deterministic step where a 'Nothing' means we work with both 'True'
-- and 'False'.
nstep
  :: Three a  -- ^ The given 'Three' where we make a non-determinstic step.
  -> Maybe Bool  -- ^ The step that we make, this can be 'Nothing' if we want to query both 'True' and 'False'.
  -> [Three a]  -- ^ The list of the possible 'Three's with the step.
nstep thr pth = nstep' thr pth []

-- | Take a sequence of steps with the given list of 'Bool's.
walk
  :: Three a  -- ^ The given 'Three' object where we make a walk.
  -> [Bool]  -- ^ A list of 'Bool's that determine for each step if we take the left or right subthree.
  -> Three a  -- ^ The corresponding subthree. For a 'Leaf' we will each time keep returning the same leaf.
walk = foldl step

-- | Obtain the children that satisfy a given 'ThreePath'.
children
  :: ThreePath  -- ^ The given 'ThreePath' for the query.
  -> Three a  -- ^ The given 'Three' that we query.
  -> [a]  -- ^ A list of /children/ that satisfy the given 'ThreePath'.
children path thr = children' path thr []

-- | Obtain the children that satisfy the given 'ThreePath'.
children'
  :: ThreePath  -- ^ The given 'ThreePath' for the query.
  -> Three a  -- ^ The given 'Three' that we query.
  -> [a]  -- ^ The list of tail elements.
  -> [a]  -- ^ The list of /children/ followed by the given list of tail elements.
children' _ (Leaf x) = (x :)
children' (_:ys) (Link x) = children' ys x
children' (Nothing:ys) (Split la lb) = go lb . go la
  where go = children' ys
children' ~(~(Just j):ys) ~(Split la lb) = go
  where go | j = children' ys lb
           | otherwise = children' ys la

instance Applicative Three where
  pure = Leaf
  (<*>) (Leaf f) = fmap f
  (<*>) (Link f) = go
      where go (Split la lb) = Split (f <*> la) (f <*> lb)
            go l = Link (f <*> _linkLeaf l)
  (<*>) ~(Split fa fb) = go
      where go (Split xa xb) = Split (fa <*> xa) (fb <*> xb)
            go l = Split (fa <*> x') (fb <*> x') where x' = _linkLeaf l
  liftA2 f = go
      where go (Leaf x) (Leaf y) = Leaf (f x y)
            go x@(Leaf _) (Link y) = Link (go x y)
            go x@(Leaf _) (Split ya yb) = Split (go x ya) (go x yb)
            go (Link x) (Split ya yb) = Split (go x ya) (go x yb)
            go (Link x) y = Link (go x (_linkLeaf y))
            go (Split xa xb) (Split ya yb) = Split (go xa ya) (go xb yb)
            go (Split xa xb) y = Split (go xa y') (go xb y')
              where y' = _linkLeaf y

instance Arbitrary1 Three where
    liftArbitrary arb = go
      where go = frequency [(5, Leaf <$> arb), (2, Link <$> go), (1, Split <$> go <*> go)]

instance Arbitrary a => Arbitrary (Three a) where
    arbitrary = arbitrary1
