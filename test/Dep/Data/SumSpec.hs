{-# LANGUAGE TypeApplications #-}

module Dep.Data.SumSpec where

import Dep.CoreTest(testBinaryLaws)

import Dep.Data.LogicItem(ToCompact(fromCompact, toCompact))
import Dep.Data.ThreeValue(ThreeValue(DontCare))
import Dep.Data.Sum(Sum, Sum', CompactSum, ProductOfSums)

import Test.Hspec(Spec, it)
import Test.QuickCheck(property)

spec :: Spec
spec = do
    it "identity of toCompact and fromCompact" (property testCompactIdentity)
    testBinaryLaws @Sum
    testBinaryLaws @CompactSum
    testBinaryLaws @ProductOfSums

testCompactIdentity :: Sum' -> Bool
testCompactIdentity ss = reverse (dropWhile (DontCare ==) (reverse ss)) == fromCompact (toCompact ss)
