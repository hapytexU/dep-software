{-# LANGUAGE OverloadedStrings #-}

module Dep.Bricks.Utils where

import Data.Text(Text, cons, pack, singleton, unpack)
import qualified Data.Text as T

import Dep.Utils(udiv)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<->), (<|>), char, imageWidth, imageHeight, text', vertCat)

arrow :: Char -> Text -> Char -> Attr -> Int -> Image
arrow c0 ci cn = go
  where go atr n = text' atr (cons c0 (T.take n (T.replicate (udiv n (T.length ci)) ci) <> singleton cn))

arrow' :: Char -> String -> Char -> Attr -> Int -> Image
arrow' c0 ci = arrow c0 (pack ci)

vline :: Text -> Attr -> Int -> Image
vline = vline' . unpack

vline' :: String -> Attr -> Int -> Image
vline' ci = go
  where go atr n = vertCat (map (char atr) (take n (cycle ci)))

inRaster :: Attr -> Image -> Image
inRaster atr img = top <-> (lft <|> img <|> rght) <-> bot
    where w = imageWidth img
          h = imageHeight img
          lft = vline "\x2503\x2520" atr h
          rght = vline "\x2503\x2528" atr h
          top = arrow '\x250f' "\x2501\x252f" '\x2513' atr w
          bot = arrow '\x2517' "\x2501\x2537" '\x251b'atr  w
