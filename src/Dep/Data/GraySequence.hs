{-# LANGUAGE Safe #-}

{-|
Module      : Dep.Data.GraySequence
Description : A module to generate /Gray/ sequences: a sequence of words where each time one bit is toggled.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A /Gray counter/ is a counter that for each step will toggle /one/ bit and eventually will yield every possible
word with the given size.
-}

module Dep.Data.GraySequence (
    -- * Sucessor of the Gray counter
    grayInc
  ) where

import Data.Bits(Bits, (.&.), complement, complementBit, popCount, shiftL, xor)

-- | Perform an increment by toggling only one bit (as a Gray counter is supposed to do).
grayInc :: (Bits a, Ord a, Num a, Enum a)
    => Int -- ^ The given number of bits of the counter.
    -> a -- ^ The given initial value of the counter.
    -> a -- ^ The resulting value of the counter after increment.
grayInc n b | even (popCount b) = complementBit b 0
            | lst > lbm = xor b (shiftL lbm 1)
            | otherwise = 0
    where lbm = b .&. succ (complement b)
          lst = shiftL 1 (pred n)
