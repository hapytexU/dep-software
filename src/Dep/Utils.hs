{-# LANGUAGE Safe, TupleSections #-}

{-|
Module      : Dep.Utils
Description : A module that defines helper functions and data structures.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines utility functions that are used elsewhere in the software package.
-}

module Dep.Utils (
    -- * List processing
    toList', zipWithLast
    -- * Lifting objects
  , applyExp, applyExp'
    -- * 'Maybe' utils
  , unionMaybeWith
    -- * Upperbound of a division
  , udiv
    -- * Raster functions
  , Raster, toRaster, flatRaster, flatRaster'
    -- * Type aliasses
  , Operator
  ) where

import Language.Haskell.TH.Lib(appE, conE)
import Language.Haskell.TH.Syntax(Exp, Lift(lift), Name, Q)

-- | Convert the given 'Foldable' object to a list where one can specify a tail list.
toList' :: Foldable f
  => f a -- ^ The 'Foldable' of elements that will yield items to the list.
  -> [a] -- ^ The list of items that will follow after the items of the 'Foldable'.
  -> [a] -- ^ A list of items from the 'Foldable' followed by the items of the tail.
toList' = flip (foldr (:))

-- | A function that create an expression by calling the function with the given name
-- and a 'Foldable' of parameters that are applied.
applyExp :: Foldable f
  => Name -- ^ The name of the function to call.
  -> f (Q Exp) -- ^ The 'Foldable' of parameters that will be used in the function call.
  -> Q Exp -- ^ An expression where a function with the given name is applied to the given 'Foldable' of the parameter.
applyExp = foldl appE . conE

-- | A function that create an expression by calling the function with the given name
-- and a 'Foldable' of items that are an instance of 'Lift'. The /lifted/ version of these
-- items will be used to construct the expression.
applyExp' :: (Lift a, Foldable f)
  => Name -- ^ The name of the function to call.
  -> f a -- ^ The 'Foldable' of items that will be lifted, and used in the function call.
  -> Q Exp -- ^ An expression where a function with the given name is applied to the given 'Foldable' of the parameter.
applyExp' = foldl ((. lift) . appE) . conE

-- | Perform a 'zipWith', but when one of the lists is exhausted
-- the remaining elements of the other list are returned.
{-# NOINLINE [1] zipWithLast #-}
zipWithLast
  :: (a -> a -> a)  -- ^ The given /merge/ function.
  -> [a]  -- ^ The first given list of items.
  -> [a]  -- ^ The second given list of items.
  -> [a]  -- ^ The result of zipping the two lists until one of the lists is exhausted and then return the remaining items of the other list.
zipWithLast f = go
  where go [] ys = ys
        go xs [] = xs
        go ~(x:xs) ~(y:ys) = f x y : go xs ys

-- | Using the merge function if the two given 'Maybe's
-- use the 'Just' data constructor. If one of the two is 'Nothing',
-- then the item with the 'Just' compiler is used. If the
-- two are both 'Nothing', 'Nothing' is returned.
unionMaybeWith
  :: (a -> a -> a) -- ^ The function to merge two 'Just' objects.
  -> Maybe a -- ^ The first given 'Maybe'
  -> Maybe a -- ^ The second given 'Maybe'
  -> Maybe a -- ^ The result of merging two 'Just' objects, or the 'Just' object if there is only one given.
unionMaybeWith f = go
  where go (Just x) (Just y) = Just (f x y)
        go x@(Just _) ~Nothing = x
        go ~Nothing j = j

-- | Divide the numerator by the denominator and round up.
udiv :: Integral i
  => i  -- ^ The given numerator.
  -> i  -- ^ The given denominator.
  -> i  -- ^ The corresponding division rounded up.
udiv n d = div (n+d-1) d

-- | A 'Raster' is a list of lists of items. This is for example used
-- to render an image with /Bricks/ where there are multiple layers.
type Raster a = [[a]]


-- | Convert the given 'Raster's to a 'Raster' of 2-tuples where the first
-- item is a given "label" or "attribute".
toRaster
  :: a  -- ^ The given /label/ or /attribute/ that will be attached to the 'Raster'.
  -> Raster b  -- ^ The given 'Raster' of values that will be annotated.
  -> Raster (a, b)  -- ^ A 'Raster' of 2-tuples where we annotate each item with the given /label/ or /attribute/ value.
toRaster x = map (map (x, ))

-- | Convert a list of labeled 'Raster's to a single 'Raster' where the predicate determines from which value we take the item.
flatRaster
  :: (b -> Bool)  -- ^ The predicate that returns 'True' if we want to retain the item from the /upper/ layer, and 'False' if we want to retain the item from the /lower/ layer.
  -> [(a, Raster b)]  -- ^ A list of annotated 'Raster's that will be merged to a signle 'Raster'.
  -> Raster (a, b)  -- ^ A single 'Raster' where each item is annotated with the /label/ or /attribute/.
flatRaster cond = flatRaster' (cond . snd) . map (uncurry toRaster)

-- | Convert the given 'Raster's to a raster where the given predicate
-- decides if we retain the upper or lower layer.
flatRaster'
  :: (a -> Bool)  -- ^ The given predicate that returns 'True' if we want to retain the upper 'Raster', and 'False' if we want to retain the lower 'Raster'.
  -> [Raster a]  -- ^ The list of 'Raster's that is ordered top-to-bottom.
  -> Raster a  -- ^ The resulting 'Raster' where we perfor the merge between all the 'Raster's.
flatRaster' f = foldr (mergeRaster' f) [[]]

-- | Merge two 'Raster's together to a new 'Raster'.
mergeRaster'
  :: (a -> Bool)  -- ^ The given predicate that returns 'True' if we want to retain the upper 'Raster' for that item, and 'False' if we want to retain the item of the lower 'Raster'.
  -> Raster a  -- ^ The given /upper/ 'Raster'.
  -> Raster a  -- ^ The given /lower/ 'Raster'.
  -> Raster a  -- ^ The raster we obtain after merging the /upper/ and /lower/ raster.
mergeRaster' p = go
  where go = zipWithLast (zipWithLast g)
          where g x y
                  | p x = x
                  | otherwise = y

-- | An alias for an /operator/: a function that takes two
-- items of the same type, and produces an item. In case
-- the function is /associative/, this can be used for a 'Semigroup'.
type Operator a = (a, a) -> a
