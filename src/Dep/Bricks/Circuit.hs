{-|
Module      : Dep.Bricks.Circuit
Description : A module that exports widgets to render circuits effectively.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render circuits in an effective manner.
-}

module Dep.Bricks.Circuit where

import Brick.Types(Size(Fixed), Widget(Widget), emptyResult, imageL) -- attrL, emptyResult, getContext, imageL)

import Control.Lens.Operators((&), (.~)) -- , (^.))

-- import Dep.Bricks.Gate(genericGate)
-- import Dep.Bricks.Layout(CircuitLayout(Horizontal))
import Dep.Bricks.Karnaugh(renderKarnaugh)
import Dep.Data.Three(Three(Leaf, Split))
import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare))

import Graphics.Vty.Attributes(defAttr)

-- | A widget that is used to render electronics circuitry.
circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  -- c <- getContext
  -- let a = c ^. attrL
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
--  return (emptyResult & imageL .~ genericGate '&' Horizontal [False, True, False, True] True a)
  let l0 = Leaf Zero
  let l1 = Leaf One
  let ld = Leaf DontCare
  let la = Leaf 'A'
  let lb = Leaf 'B'
  let lc = Leaf 'C'
  let ld = Leaf 'D'
  let le = Leaf 'E'
  let lf = Leaf 'F'
  let lg = Leaf 'G'
  let lh = Leaf 'H'
  let li = Leaf 'I'
  let lj = Leaf 'J'
  let lk = Leaf 'K'
  let ll = Leaf 'L'
  let lm = Leaf 'M'
  let ln = Leaf 'N'
  let lo = Leaf 'O'
  let lp = Leaf 'P'
--   return (emptyResult & imageL .~ renderKarnaugh' (Split (Split (Split (Split l1 l0) l1) (Split (Split l0 l1) l1)) (Split (Split l1 ld) ld)) defAttr)
  return (emptyResult & imageL .~ renderKarnaugh (Split (Split (Split (Split la lb) (Split lc ld)) (Split (Split le lf) (Split lg lh))) (Split (Split (Split li lj) (Split lk ll)) (Split (Split lm ln) (Split lo lp)))) mempty defAttr)
