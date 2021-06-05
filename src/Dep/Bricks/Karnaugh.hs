{-# LANGUAGE Safe #-}

{-|
Module      : Dep.Bricks.Karnaugh
Description : A module to define three-value logic.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to render, update, and interact with Karnaugh cards.
-}

module Dep.Bricks.Karnaugh where

import Dep.Class.Renderable(CharRenderable)
import Dep.Data.Three(Three)

renderKarnaugh :: CharRenderable a => Three a -> Bool
renderKarnaugh = undefined
