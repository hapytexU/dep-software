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
  let la = Leaf 'a'
  let lb = Leaf 'b'
  let lc = Leaf 'c'
  let ld = Leaf 'd'
  let le = Leaf 'e'
  let lf = Leaf 'f'
  let lg = Leaf 'g'
  let lh = Leaf 'h'
  let li = Leaf 'i'
  let lj = Leaf 'j'
  let lk = Leaf 'k'
  let ll = Leaf 'l'
  let lm = Leaf 'm'
  let ln = Leaf 'n'
  let lo = Leaf 'o'
  let lp = Leaf 'p'

  let lA = Leaf 'A'
  let lB = Leaf 'B'
  let lC = Leaf 'C'
  let lD = Leaf 'D'
  let lE = Leaf 'E'
  let lF = Leaf 'F'
  let lG = Leaf 'G'
  let lH = Leaf 'H'
  let lI = Leaf 'I'
  let lJ = Leaf 'J'
  let lK = Leaf 'K'
  let lL = Leaf 'L'
  let lM = Leaf 'M'
  let lN = Leaf 'N'
  let lO = Leaf 'O'
  let lP = Leaf 'P'

  let tree' = Split (Split (Split (Split la lb) (Split lc ld)) (Split (Split le lf) (Split lg lh))) (Split (Split (Split li lj) (Split lk ll)) (Split (Split lm ln) (Split lo lp)))
  let tree'' = Split (Split (Split (Split lA lB) (Split lC lD)) (Split (Split lE lF) (Split lG lH))) (Split (Split (Split lI lJ) (Split lK lL)) (Split (Split lM lN) (Split lO lP)))
  let tree''' = Split tree' tree''
  -- let tree'''' = Split tree''' tree'''
  -- let tree''''' = Split tree'''' tree''''
  -- let tree'''''' = Split tree''''' tree''''' --}
  let tree = tree''' -- Split tree'''''' tree''''''

  -- let tree = Split (Split (Split (Split la lb) (Split lc ld)) (Split (Split le lf) (Split lg lh))) (Split (Split (Split li lj) (Split lk ll)) (Split (Split lm ln) (Split lo lp)))
--   return (emptyResult & imageL .~ renderKarnaugh' (Split (Split (Split (Split l1 l0) l1) (Split (Split l0 l1) l1)) (Split (Split l1 ld) ld)) defAttr)
  return (emptyResult & imageL .~ renderKarnaugh tree mempty undefined defAttr)
