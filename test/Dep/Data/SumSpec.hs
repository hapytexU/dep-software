{-# LANGUAGE RankNTypes, TypeApplications #-}

module Dep.Data.SumSpec where

import Dep.CoreTest(testBinaryLaws)

import Data.Bits(shiftL)
import Data.Word(Word64)

import Dep.Data.LogicItem(EvaluateItem(isTrivial, evaluateWithBits, numberOfVariables), ToCompact(fromCompact, toCompact))
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One))
import Dep.Data.Sum(Sum, Sum', CompactSum, ProductOfSums)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

spec :: Spec
spec = do
    it "identity of toCompact and fromCompact" (property testCompactIdentity)
    testBinaryLaws @Sum
    testBinaryLaws @CompactSum
    testBinaryLaws @ProductOfSums
    it "isTrivial" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (testTrivialItem @ Sum)))
    it "isTrivial" (quickCheckWith stdArgs { maxSuccess = 10000000 } (property (testTrivialItem @ CompactSum)))
    it "isTrivial" (quickCheckWith stdArgs { maxSuccess = 1000000 } (property (testTrivialItem @ ProductOfSums)))

testCompactIdentity :: Sum' -> Bool
testCompactIdentity ss = reverse (dropWhile (DontCare ==) (reverse ss)) == fromCompact (toCompact ss)

testTrivialItem :: forall a . EvaluateItem a => a -> Bool
testTrivialItem ei = nvar >= 10 || go (isTrivial ei)
  where go One = and mvs
        go Zero = all not mvs
        go DontCare = True `elem` mvs && False `elem` mvs
        nvar = numberOfVariables ei
        mvs = map (`evaluateWithBits` ei) [0 :: Word64 .. shiftL 1 nvar]
