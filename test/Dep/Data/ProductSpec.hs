{-# LANGUAGE TypeApplications #-}

module Dep.Data.ProductSpec where

import Dep.CoreTest(testBinaryLaws)

import Dep.Data.LogicItem(ToCompact(fromCompact, toCompact))
import Dep.Data.Product(Product, Product', CompactProduct, SumOfProducts)
import Dep.Data.ThreeValue(ThreeValue(DontCare))

import Test.Hspec(Spec, it)
import Test.QuickCheck(property)

spec :: Spec
spec = do
    it "identity of toCompact and fromCompact" (property testCompactIdentity)
    testBinaryLaws @Product
    testBinaryLaws @CompactProduct
    testBinaryLaws @SumOfProducts

testCompactIdentity :: Product' -> Bool
testCompactIdentity ps = reverse (dropWhile (DontCare ==) (reverse ps)) == fromCompact (toCompact ps)
