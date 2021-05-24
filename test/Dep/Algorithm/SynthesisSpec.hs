module Dep.Algorithm.SynthesisSpec where

import Data.Bits(shiftL, testBit)
import Data.Foldable(toList)
import Data.Word(Word64)

import Dep.Class.Walkable(walk)
import Dep.Algorithm.Synthesis(synthesisSOP, synthesisPOS)
import Dep.Data.LogicItem(EvaluateItem(evaluateWithBits))
import Dep.Data.Three(Three, depth)
import Dep.Data.ThreeValue(ThreeValue, fromBool)

import Test.Hspec(Spec, it)
import Test.QuickCheck(maxSuccess, property, quickCheckWith, stdArgs)

spec :: Spec
spec = do
  it "depth check" (property depthCheck)
  it "synthesis check SOP" (quickCheckWith stdArgs { maxSuccess = 1000000 } (property synthesisCheckSop))
  it "synthesis check POS" (quickCheckWith stdArgs { maxSuccess = 1000000 } (property synthesisCheckPos))

depthCheck :: Three ThreeValue -> Bool
depthCheck thr = depth thr >= 0

synthesisCheckSop :: Three ThreeValue -> Bool
synthesisCheckSop = synthesisCheck synthesisSOP

synthesisCheckPos :: Three ThreeValue -> Bool
synthesisCheckPos = synthesisCheck synthesisPOS


synthesisCheck :: EvaluateItem a => (Three ThreeValue -> a) -> Three ThreeValue -> Bool
synthesisCheck f thr = depth thr >= 63 || all go [1 .. mx]
  where dpth = depth thr
        mx = shiftL 1 dpth :: Word64
        syn = f thr
        go x = all ((thes ==) . (<> thes)) (toList (walk thr (map (testBit x) [0 .. dpth])))
          where thes = fromBool (evaluateWithBits x syn)
