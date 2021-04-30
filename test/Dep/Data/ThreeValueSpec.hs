{-# LANGUAGE TypeApplications #-}

module Dep.Data.ThreeValueSpec where

import Dep.CoreTest(testSemigroupLaws, testMonoidLaws)
import Dep.Data.ThreeValue(ThreeValue)

import Test.Hspec

spec :: Spec
spec = do
    testSemigroupLaws @ ThreeValue
    testMonoidLaws @ ThreeValue
