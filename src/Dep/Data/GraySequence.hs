module Dep.Data.GraySequence where

import Data.Bits(Bits, (.&.), complement, complementBit, popCount, shiftL, xor)

newtype Gray a = Gray a

-- | Perform an increment by toggling only one bit (as a Gray counter is supposed to do).
grayInc :: (Bits a, Ord a, Num a, Enum a) => Int -- ^ The given number of bits of the counter.
    -> a -- ^ The given initial value of the counter.
    -> a -- ^ The resulting value of the counter after increment.
grayInc n b | even (popCount b) = complementBit b 0
            | lst > lbm = xor b (shiftL lbm 1)
            | otherwise = 0
    where lbm = b .&. succ (complement b)
          lst = shiftL 1 $ pred n
