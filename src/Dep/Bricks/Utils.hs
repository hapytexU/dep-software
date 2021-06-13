{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Dep.Bricks.Utils
Description : A module that provides utility functions to render 'Image's.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that is used to render lines, arrows, rasters, etc.
-}

module Dep.Bricks.Utils (
    -- * Lines
    hline, hline'
  , vline, vline'
    -- * Arrows
  , harrow, harrow'
  , varrow, varrow'
    -- * Raster (for Karnaugh cards)
  , inRaster
  ) where

import Data.Text(Text, cons, pack, singleton, unpack)
import qualified Data.Text as T

import Dep.Utils(udiv)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, imageWidth, imageHeight, text', vertCat)

-- | Render a /horizontal/ arrow with the given number of characters between the arrow heads. The 'Text'
-- in the middle is "/cycled/".
harrow
  :: Char  -- ^ The left arrow head.
  -> Text  -- ^ The 'Text' in the middle that is constantly repeated.
  -> Char  -- ^ The right arrow head.
  -> Attr  -- ^ The 'Attr'ibute to specify the style of the arrow.
  -> Int  -- ^ The number characters between the arrow heads (so we are /not/ counting the arrow heads).
  -> Image  -- ^ An image that shows a horizontal arrow with a given number of characters in the middle.
harrow c0 ci cn = go
  where go atr n = text' atr (cons c0 (T.take n (T.replicate (udiv n (T.length ci)) ci) <> singleton cn))

-- | A function equivalent to 'harrow', but where we use a 'String' to specify the 'Char'acters in the middle.
harrow'
  :: Char  -- ^ The left arrow head.
  -> String -- ^ The 'String' in the middle that is constantly repeated.
  -> Char  -- ^ The right arrow head.
  -> Attr  -- ^ The 'Attr'ibute to specify the style of the arrow.
  -> Int  -- ^ The number characters between the arrow heads (so we are /not/ counting the arrow heads).
  -> Image  -- ^ An image that shows a horizontal arrow with a given number of characters in the middle.
harrow' c0 = harrow c0 . pack

-- | Render a /vertical/ arrow with the given number of characters between the arrow heads. The 'Text'
-- in the middle is "/cycled/".
varrow
  :: Char  -- ^ The top arrow head.
  -> Text  -- ^ The 'Text' in the middle that is constantly repeated.
  -> Char  -- ^ The bottom arrow head.
  -> Attr  -- ^ The 'Attr'ibute to specify the style of the arrow.
  -> Int  -- ^ The number characters between the arrow heads (so we are /not/ counting the arrow heads).
  -> Image  -- ^ An image that shows a vertical arrow with a given number of characters in the middle.
varrow c0 = varrow' c0 . unpack

-- | A function equivalent to 'varrow', but where we use a 'String' to specify the 'Char'acters in the middle.
varrow'
  :: Char  -- ^ The top arrow head.
  -> String  -- ^ The 'String' in the middle that is constantly repeated.
  -> Char  -- ^ The bottom arrow head.
  -> Attr  -- ^ The 'Attr'ibute to specify the style of the arrow.
  -> Int  -- ^ The number characters between the arrow heads (so we are /not/ counting the arrow heads).
  -> Image  -- ^ An image that shows a vertical arrow with a given number of characters in the middle.
varrow' c0 ci cn = go
  where go atr n = vertCat (map (char atr) (c0 : take n (cycle ci) <> [cn]))

-- | Render a /horizontal/ line by cycling through the given 'Text' and apply the given 'Attr'ibute to
-- the result.
hline
  :: Text  -- ^ The 'Text' object that determines how to render the line. The items will be /cycled/.
  -> Attr  -- ^ The 'Attr'ibute that determines how to render the line.
  -> Int  -- ^ The given length of the line.
  -> Image  -- ^ An 'Image' that contains a /horizontal/ line with the given length.
hline ci = go
  where go atr n = text' atr (T.take n (T.replicate (udiv n (T.length ci)) ci))

-- | A function equivalent to 'hline', but with a 'String' to specify the 'Char'acters instead of a 'Text' object.
hline'
  :: String  -- ^ The 'String' object that determines how to render the line. The items will be /cycled/.
  -> Attr  -- ^ The 'Attr'ibute that determines how to render the line.
  -> Int  -- ^ The given length of the line.
  -> Image  -- ^ An 'Image' that contains a /horizontal/ line with the given length.
hline' = hline . pack

-- | Render a /vertical/ line by cycling through the given 'Text' and apply the given 'Attr'ibute to
-- the result.
vline
  :: Text  -- ^ The 'String' object that determines how to render the line. The items will be /cycled/.
  -> Attr  -- ^ The 'Attr'ibute that determines how to render the line.
  -> Int  -- ^ The given length of the line.
  -> Image  -- ^ An 'Image' that contains a /vertical/ line with the given length.
vline = vline' . unpack

-- | A function equivalent to 'vline', but with a 'String' to specify the 'Char'acters instead of a 'Text' object.
vline'
  :: String  -- ^ The 'String' object that determines how to render the line. The items will be /cycled/.
  -> Attr  -- ^ The 'Attr'ibute that determines how to render the line.
  -> Int  -- ^ The given length of the line.
  -> Image  -- ^ An 'Image' that contains a /vertical/ line with the given length.
vline' ci = go
  where go atr n = vertCat (map (char atr) (take n (cycle ci)))

-- | Wrap the given 'Image' in a raster structure with thick borders
-- and with small lines for the raster image in the middle.
inRaster
  :: Attr  -- ^ The 'Attr'ibute that specifies how to render the raster border.
  -> Image  -- ^ The 'Image' that we want to wrap in a /raster/.
  -> Image  -- ^ An 'Image' that contains the given image wrapped in a /raster/.
inRaster atr img = top <-> (lft <|> img <|> rght) <-> bot
    where w = imageWidth img
          h = imageHeight img
          lft = vline "\x2503\x2520" atr h
          rght = vline "\x2503\x2528" atr h
          top = harrow '\x250f' "\x2501\x252f" '\x2513' atr w
          bot = harrow '\x2517' "\x2501\x2537" '\x251b'atr  w
