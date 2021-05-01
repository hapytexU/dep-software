{-# LANGUAGE TypeApplications #-}

module Dep.Data.ProductSpec where

import Dep.Data.ThreeValue(ThreeValue(DontCare))
import Dep.Data.Product(Product', toCompact, fromCompact)

import Test.Hspec(Spec, it)
import Test.QuickCheck(property)

spec :: Spec
spec = do
    it "identity of toCompact and fromCompact" (property testCompactIdentity)

testCompactIdentity :: Product' -> Bool
testCompactIdentity ps = reverse (dropWhile (DontCare ==) (reverse ps)) == fromCompact (toCompact ps)
