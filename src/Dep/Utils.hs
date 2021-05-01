{-# LANGUAGE Safe #-}

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
