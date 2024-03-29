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
-- import Dep.Data.ThreeValue(ThreeValue(Zero, One, DontCare))

import Graphics.Vty.Image((<|>), emptyImage)
import Graphics.Vty.Attributes(defAttr)

-- | A widget that is used to render electronics circuitry.
circuit :: Widget ()
circuit = Widget Fixed Fixed $ do
  -- c <- getContext
  -- let a = c ^. attrL
  -- return (emptyResult & imageL .~ andGateV3 (c ^. attrL))
--  return (emptyResult & imageL .~ genericGate '&' Horizontal [False, True, False, True] True a)
  -- let l0 = Leaf Zero
  -- let l1 = Leaf One
  -- let ld = Leaf DontCare
  let l = Leaf
  let l0 = l '0'
  let l1 = l '1'
  let l2 = l '2'
  let l3 = l '3'
  let l4 = l '4'
  let l5 = l '5'
  let l6 = l '6'
  let l7 = l '7'
  let l8 = l '8'
  let l9 = l '9'
  let la = l 'a'
  let lb = l 'b'
  let lc = l 'c'
  let ld = l 'd'
  let le = l 'e'
  let lf = l 'f'
  let lg = l 'g'
  let lh = l 'h'
  let li = l 'i'
  let lj = l 'j'
  let lk = l 'k'
  let ll = l 'l'
  let lm = l 'm'
  let ln = l 'n'
  let lo = l 'o'
  let lp = l 'p'
  let lq = l 'q'
  let lr = l 'r'
  let ls = l 's'
  let lt = l 't'
  let lu = l 'u'
  let lv = l 'v'

  let t0 = l0
  let t1 = Split l0 l1
  let t2 = Split t1 (Split l2 l3)
  let t3 = Split t2 (Split (Split l4 l5) (Split l6 l7))
  let t4 = Split t3 (Split (Split (Split l8 l9) (Split la lb)) (Split (Split lc ld) (Split le lf)))
  let t5 = Split t4 (Split (Split (Split (Split lg lh) (Split li lj)) (Split (Split lk ll) (Split lm ln))) (Split (Split (Split lo lp) (Split lq lr)) (Split (Split ls lt) (Split lu lv))))
  let t6 = Split t5 t5
  let t7 = Split t6 t6
  let t8 = Split t7 t7
  let t9 = Split t8 t8
  let ta = Split t9 t9
  let tb = Split ta ta
  let g t = renderKarnaugh t mempty ["a", "g", "c"] defAttr

  -- let tree' = Split (Split (Split (Split la lb) (Split lc ld)) (Split (Split le lf) (Split lg lh))) (Split (Split (Split li lj) (Split lk ll)) (Split (Split lm ln) (Split lo lp)))
  -- let tree'' = Split (Split (Split (Split lA lB) (Split lC lD)) (Split (Split lE lF) (Split lG lH))) (Split (Split (Split lI lJ) (Split lK lL)) (Split (Split lM lN) (Split lO lP)))
  -- let tree''' = Split tree' tree''
  -- let tree'''' = Split tree''' tree'''
  -- let tree''''' = Split tree'''' tree''''
  -- let tree'''''' = Split tree''''' tree''''' --}
  let tree = t2 -- tree''' -- Split tree'''''' tree''''''

  -- let tree = Split (Split (Split (Split la lb) (Split lc ld)) (Split (Split le lf) (Split lg lh))) (Split (Split (Split li lj) (Split lk ll)) (Split (Split lm ln) (Split lo lp)))
--   return (emptyResult & imageL .~ renderKarnaugh' (Split (Split (Split (Split l1 l0) l1) (Split (Split l0 l1) l1)) (Split (Split l1 ld) ld)) defAttr)
  return (emptyResult & imageL .~ (foldr ((<|>) . g) emptyImage (drop 0 [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, ta, tb])))
