{-# LANGUAGE DefaultSignatures, Safe #-}

{-|
Module      : Dep.Class.Renderable
Description : A module that has a type class to render an object.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that has a type class to render items of the types that are members of the typeclass.
-}


module Dep.Class.Renderable (
    CharRenderable(charRenderItem), Renderable(renderItem)
  ) where

import Data.Bool(bool)
import Data.Text(Text, singleton)

-- | A typeclass that specifies that a certain type can be rendered with a single 'Char'acter.
class CharRenderable a where
  -- | Render the given item to a 'Char' object.
  charRenderItem
    :: a  -- ^ THe given item to render.
    -> Char  -- ^ The corresponding 'Char'acter when rendering the object.
  {-# MINIMAL charRenderItem #-}

-- | A typeclass that specifies that a certain type can be rendered as a DEP 'Text' object.
class Renderable a where
  -- | Render the given item to a 'Text' object.
  renderItem
    :: a -- ^ The given item to render.
    -> Text  -- ^ The corresponding 'Text' for the rendered information.
  default renderItem :: CharRenderable a => a -> Text
  renderItem = singleton . charRenderItem

instance CharRenderable Bool where
  charRenderItem = bool '0' '1'

instance Renderable Bool

instance CharRenderable Char where
  charRenderItem = id

instance Renderable Char

instance CharRenderable a => CharRenderable (Maybe a) where
  charRenderItem = maybe '-' charRenderItem

instance CharRenderable a => Renderable (Maybe a)
