{-# LANGUAGE TypeApplications #-}

module Dep.Data.ThreeSpec where

import Dep.CoreTest(testFunctorLaws, testApplicativeLaws, testMonadLaws, testSemigroupLaws, testMonoidLaws)
import Dep.Data.Three(Three)

import Test.Hspec
-- import Test.QuickCheck


spec :: Spec
spec = do
    testFunctorLaws @ Three @ Char @ Int @ String
    testApplicativeLaws @ Three @ Char @ Int @ String
    testSemigroupLaws @ (Three Int)
    testMonoidLaws @ (Three Int)
