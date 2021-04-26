{-# LANGUAGE TypeApplications #-}

module Dep.Data.ThreeSpec where

import Dep.CoreTest(testFunctorLaws, testApplicativeLaws, testMonadLaws)
import Dep.Data.Three(Three)

import Test.Hspec
-- import Test.QuickCheck


spec :: Spec
spec = do
    testFunctorLaws @ Three @ Char @ Int @ String
    testApplicativeLaws @ Three @ Char @ Int @ String
    testMonadLaws @ Three @ Char @ Int @ String
