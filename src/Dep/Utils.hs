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
    toList'
    -- * Lifting objects
  , applyExp, applyExp'
    -- * 'Maybe' utils
  , unionMaybeWith
    -- * Upperbound of a division
  , udiv
    -- * Raster functions
  , toRaster, flatRaster, flatRaster'
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

type Raster a = [[a]]

toRaster :: a -> Raster b -> Raster (a, b)
toRaster x = map (map (x, ))

flatRaster :: (b -> Bool) -> [(a, Raster b)] -> Raster (a, b)
flatRaster cond = flatRaster' (cond . snd) . map (uncurry toRaster)

flatRaster' :: (a -> Bool) -> [Raster a] -> Raster a
flatRaster' = undefined
