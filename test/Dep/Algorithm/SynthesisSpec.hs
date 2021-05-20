module Dep.Algorithm.SynthesisSpec where

import Data.Bits(shiftL, testBit)
import Data.Foldable(toList)
import Data.Word(Word64)

import Dep.Class.Walkable(walk)
import Dep.Algorithm.Synthesis(synthesis)
import Dep.Data.LogicItem(evaluateWithBits)
import Dep.Data.Three(Three, depth)
import Dep.Data.ThreeValue(ThreeValue, fromBool)

import Test.Hspec(Spec, it)
import Test.QuickCheck(property)

spec :: Spec
spec = do
  it "depth check" (property depthCheck)
  it "synthesis check" (property synthesisCheck)

depthCheck :: Three ThreeValue -> Bool
depthCheck thr = depth thr >= 0

synthesisCheck :: Three ThreeValue -> Bool
synthesisCheck thr = depth thr >= 63 || all go [1 .. mx]
  where dpth = depth thr
        mx = shiftL 1 dpth :: Word64
        syn = synthesis thr
        go x = all ((thes ==) . (<> thes)) (toList (walk thr (map (testBit x) [0 .. dpth])))
          where thes = fromBool (evaluateWithBits x syn)


-- simpleThree :: Three ThreeValue
-- simpleThree =
