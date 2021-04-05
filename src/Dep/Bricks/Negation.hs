{-|
Module      : Dep.Bricks.Negation
Description : A module to create images with /negations/ for the /gate/ and /wires/.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to create images with negations for gates, wires, and other modules.
-}

module Dep.Bricks.Negation (
    -- * Negation for a single wire
    negation, negationH, negationV
    -- * Negation for a list of wires
  , negationList, negationHList, negationVList
  ) where

import Data.Bool(bool)

import Dep.Bricks.Box(negator, linev, lineh)

import Graphics.Vty.Attributes(Attr)
import Graphics.Vty.Image(Image, (<|>), (<->), char, emptyImage)

-- | Create an Image that contains a single character that is either the 'negator' or the given 'Char'.
negation
  :: Char  -- ^ The 'Char' to use in case we do not negate.
  -> Bool  -- ^ A 'Bool' that specifies if this is a negator.
  -> Attr  -- ^ The 'Attr' to use when rendering the image.
  -> Image  -- ^ The corresponding 'Image' for the given (optional) negator.
negation c = flip char . bool c negator

-- | Create an image for a negator on a /horizontal/ wire.
negationH
  :: Bool  -- ^ Specifies if this is a simple wire ('False'), or a negator ('True').
  -> Attr  -- ^ The 'Attr' to use when rendering the image.
  -> Image  -- ^ The corresponding 'Image' for the given (optional) negator.
negationH = negation linev

-- | Create an image for a negator on a /vertical/ wire.
negationV
  :: Bool  -- ^ Specifies if this is a simple wire ('False'), or a negator ('True').
  -> Attr  -- ^ The 'Attr' to use when rendering the image.
  -> Image  -- ^ The corresponding 'Image' for the given (optional) negator.
negationV = negation lineh

-- | Create a list of negators in one of the two directions. The 'Char' specifies
-- what to render in case it is not a negator.
negationList
  :: (Image -> Image -> Image)  -- ^ A function to merge two 'Image's together.
  -> Char  -- ^ A 'Char' that specifies what to render in case we do not render a negator.
  -> [Bool]  -- ^ The list of 'Bool's that specifies when a negator occurs.
  -> Attr  -- ^ The 'Attr' that specifies the style of the negators.
  -> Image  -- ^ The corresponding 'Image' that contains wires and negators.
negationList mgr lns bls attr = foldr (mgr . (`go` attr)) emptyImage bls
  where go = negation lns

-- | Create an 'Image' of negators in the /horizontal/ direction. The wires here are in the /vertical/ direction.
negationHList
  :: [Bool]  -- ^ The list of 'Bool's that specifies when a negator occurs.
  -> Attr  -- ^ The 'Attr' that specifies the style of the negators.
  -> Image  -- ^ The corresponding 'Image' that contains wires and negators.
negationHList = negationList (<|>) linev

-- | Create an 'Image' of negators in the /vertical/ direction. The wires here are in the /horizontal/ direction.
negationVList
  :: [Bool]  -- ^ The list of 'Bool's that specifies when a negator occurs.
  -> Attr  -- ^ The 'Attr' that specifies the style of the negators.
  -> Image  -- ^ The corresponding 'Image' that contains wires and negators.
negationVList = negationList (<->) lineh
