{-# LANGUAGE TypeApplications #-}

module Dep.Data.ThreeSpec where

import Dep.CoreTest(testFunctor)
import Dep.Data.Three(Three)

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
    testFunctor @ Three @ Char @ Int @ String
