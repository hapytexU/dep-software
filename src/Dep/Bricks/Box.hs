{-|
Module      : Dep.Bricks.Box
Description : A module to define characters that are used in the other modules for the /bricks/.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines characters to generate images in the user interface. These render wires, boxes, etc.
-}

module Dep.Bricks.Box (
    -- * Box borders
    boxh,  boxv
  , boxlb, boxlt, boxrb, boxrt
  , boxhu, boxhd, boxvl, boxvr
    -- * Wire characters
  , lineh, linev, negator
  ) where

-- | A character to render a /wire/ in a /vertical/ direction.
linev :: Char
linev = '\x2502'

-- | A character to render a /wire/ in a /horizontal/ direction.
lineh :: Char
lineh = '\x2500'

-- | A character to render the top left corner of a /thick/ box.
boxlt :: Char
boxlt = '\x250f'

-- | A character to render the horizontal border corner of a /thick/ box.
boxh :: Char
boxh = '\x2501'

-- | A character to render the vertical border corner of a /thick/ box.
boxv :: Char
boxv = '\x2503'

-- | A character to render the top right corner of a /thick/ box.
boxrt :: Char
boxrt = '\x2513'

-- | A character to render the bottom left corner of a /thick/ box.
boxlb :: Char
boxlb = '\x2517'

-- | A character to render the bottom right corner of a /thick/ box.
boxrb :: Char
boxrb = '\x251b'

-- | A character to render the horizontal border of a /thick/ box with a wire at at the upper part.
boxhu :: Char
boxhu = '\x2537'

-- | A character to render the horizontal border of a /thick/ box with a wire at at the bottom part.
boxhd :: Char
boxhd = '\x252f'

-- | A character to render the vertical border of a /thick/ box with a wire at at the left part.
boxvl :: Char
boxvl = '\x2528'

-- | A character to render the vertical border of a /thick/ box with a wire at at the right part.
boxvr :: Char
boxvr = '\x2520'

-- | A character to render a negator.
negator :: Char
negator = 'O'
