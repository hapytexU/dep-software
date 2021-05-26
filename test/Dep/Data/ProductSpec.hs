{-# LANGUAGE RankNTypes, TypeApplications #-}

module Dep.Data.ProductSpec where

import Data.Bits(shiftL)
import Data.Word(Word64)

import Dep.CoreTest(testBinaryLaws)

import Dep.Data.LogicItem(EvaluateItem(evaluateWithBits, isTrivial, numberOfVariables), ToCompact(fromCompact, toCompact))
import Dep.Data.Product(Product, Product', CompactProduct, SumOfProducts)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One))

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

spec :: Spec
spec = do
    it "identity of toCompact and fromCompact" (property testCompactIdentity)
    testBinaryLaws @Product
    testBinaryLaws @CompactProduct
    testBinaryLaws @SumOfProducts
    it "trivial test" (quickCheckWith stdArgs { maxSuccess = 100000000 } (property (testTrivialItem @ Product)))
    it "trivial test" (quickCheckWith stdArgs { maxSuccess = 100000000 } (property (testTrivialItem @ CompactProduct)))
    it "trivial test" (quickCheckWith stdArgs { maxSuccess = 100000000 } (property (testTrivialItem @ SumOfProducts)))

testCompactIdentity :: Product' -> Bool
testCompactIdentity ps = reverse (dropWhile (DontCare ==) (reverse ps)) == fromCompact (toCompact ps)

testTrivialItem :: forall a . EvaluateItem a => a -> Bool
testTrivialItem ei = nvar >= 10 || go (isTrivial ei)
  where go One = and mvs
        go Zero = all not mvs
        go DontCare = True `elem` mvs && False `elem` mvs
        nvar = numberOfVariables ei
        mvs = map (`evaluateWithBits` ei) [0 :: Word64 .. shiftL 1 nvar]
