{-# LANGUAGE RankNTypes, TypeApplications #-}

module Dep.Data.ThreeSpec where

import Dep.CoreTest(testFunctorLaws, testApplicativeLaws, testIdempotentLaws, testSemigroupLaws, testMonoidLaws, testBinaryLaws)
import Dep.Data.Three(Three, depth, simplify)

import Test.Hspec(Spec, it)
import Test.QuickCheck(property)

spec :: Spec
spec = do
    testFunctorLaws @ Three @ Char @ Int @ String
    testApplicativeLaws @ Three @ Char @ Int @ String
    testSemigroupLaws @ (Three [Int])
    testMonoidLaws @ (Three [Int])
    testBinaryLaws @ (Three Int)
    it "test simplification reduces height" (property (testSimplification @ Int))
    (testIdempotentLaws @ (Three Bool)) simplify

testSimplification :: forall a . Eq a => Three a -> Bool
testSimplification ts = depth (simplify ts) <= depth ts
