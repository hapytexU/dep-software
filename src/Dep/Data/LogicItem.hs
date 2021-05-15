{-# LANGUAGE BangPatterns, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, Safe #-}

{-|
Module      : Dep.Data.LogicItem
Description : A module that defines data structures for a sums, products, sum-of-products and product-of-sums.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module provides utility functions to compress/decompress sums, products, sum-of-products and product-of-sums.
-}

module Dep.Data.LogicItem (
  -- * Convert from and to a compact format
    ToCompact(toCompact, fromCompact)
  -- * Binary encode/decode 'ThreeValues' in an efficient way.
  , getThreeList, putThreeList
    -- * Typesetting variables
  , subscriptVariable, subscriptNegatedVariable, subscriptConditionVariable
  ) where

import Data.Bits((.|.), (.&.), shiftL, shiftR)
import Data.Binary(Get, Put, getWord8, putWord8)
import Data.Char.Small(asSub')
import Data.Text(Text, cons, snoc)
import Data.Word(Word8)

import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare), ThreeValues)

-- | A class that specifies that a given logic item can be made more compact, or from a compact form to
-- its original form. The two functions are not per se fully each others inverse.
class ToCompact a b | a -> b where
  -- | Convert the given item to a more compact representation.
  toCompact
    :: a  -- ^ The given item to turn in a compact form.
    -> b  -- ^ The corresponding compact form.

  -- | Convert the given item from a compact representation to its normal presentation.
  fromCompact
    :: b  -- ^ The given compact form that should be represented to the normal form.
    -> a  -- ^ The corresponding normal form.
  {-# MINIMAL toCompact, fromCompact #-}

instance ToCompact [ThreeValue] [Int] where
  toCompact = go 1
    where go _ [] = []
          go !i (DontCare:xs) = go (i+1) xs
          go i (One:xs) = i : go (i+1) xs
          go i (~Zero:xs) = -i : go (i+1) xs
  fromCompact = go 1
    where go _ [] = []
          go !i xa@(x:xs)
            | abs x /= i = DontCare : go (i+1) xa
            | x < 0 = Zero : go (i+1) xs
            | otherwise = One : go (i+1) xs

-- | Convert the given list of 'ThreeValue's to a writer for a binary stream that
-- encodes four 'ThreeValue's on one byte.
putThreeList :: ThreeValues -> Put
putThreeList = go
  where go [] = putWord8 0xff
        go ~(x:xs) = go' (fromIntegral (fromEnum x)) xs
        go' n [] = putWord8 (n .|. 0x3f)
        go' n ~(x:xs) = go'' (mv n x) xs
        go'' n [] = putWord8 (n .|. 0xf)
        go'' n ~(x:xs) = go''' (mv n x) xs
        go''' n [] = putWord8 (n .|. 0x3)
        go''' n ~(x:xs) = putWord8 (mv n x) >> go xs
        mv n x = shiftL n 2 .|. fromIntegral (fromEnum x)

-- | Read a list o 'ThreeValue's from a binary stream where each byte represents
-- (up to) four 'ThreeValue's.
getThreeList :: Get ThreeValues
getThreeList = getWord8 >>= go
  where go :: Word8 -> Get ThreeValues
        go 255 = pure []
        go n | shr n 0 /= 3 = prcs n <$> (getWord8 >>= go)
             | otherwise = pure (prcs n [])
        prcs n = tk n 6 . tk n 4 . tk n 2 . tk n 0
        shr n k = (0x03 .&. shiftR n k)
        tk n k = prc (shr n k)
        prc n | n < 3 = (toEnum (fromIntegral n) :)
              | otherwise = id

-- | Convert the given number to a 'Text' object containing the "root" variable
-- name and the index as subscript.
subscriptVariable
  :: Char  -- ^ The "root" variable that we will use with a subscript.
  -> Int  -- ^ The subscript that will be added to the character.
  -> Text -- ^ A text object that contains the "root" variable with the subscript.
subscriptVariable ci = cons ci . asSub'

-- | Convert the given number to a 'Text' object containing the "root" variable
-- name and the index as subscript.
subscriptNegatedVariable
  :: Char  -- ^ The "root" variable that we will use with a subscript.
  -> Int  -- ^ The subscript that will be added to the character.
  -> Text -- ^ A text object that contains the "root" variable with the subscript and an accent to mark its negation.
subscriptNegatedVariable ci = (`snoc` '\'') . cons ci . asSub'

-- | Convert the given number to a 'Text' objet that contains the "root" variable,
-- name, the index, and based on a given 'Bool' can include a quote mark if the
-- variable should be 'False'.
subscriptConditionVariable
  :: Char  -- ^ The "root" variable that will be used with a subscript.
  -> Int  -- ^ The subscript that will be added to the variable.
  -> Bool  -- ^ A 'Bool' that is 'False' if the item should be negated; and 'True' otherwise.
  -> Text  -- ^ A 'Text' object that contains the root variable, the subscript, and an accent if necessary.
subscriptConditionVariable c i = go
  where go True = subscriptVariable c i
        go ~False = subscriptNegatedVariable c i
