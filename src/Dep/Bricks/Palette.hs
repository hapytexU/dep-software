module Dep.Bricks.Palette (
    -- * Palettes
    Palette, isoColorPalette, brightIsoColorPalette, color240Palette
    -- * Determine other color for 'ISOColor's
  , swapIsoColorBright, isoColorToBright, isoColorFromBright
  ) where

import Graphics.Vty.Attributes.Color(Color(ISOColor, Color240))

-- | A palette is an (endless) list of 'Color's.
type Palette = [Color]

-- | Work with a color palette of the six ISO colors.
isoColorPalette
  :: Palette  -- ^ A palette that endlessly repeats itself and exhaustively enumerates the possible /ISO colors/.
isoColorPalette = cycle (ISOColor <$> [ 1 .. 6 ])

-- | Work with a color palette of the six ISO colors.
brightIsoColorPalette
  :: Palette  -- ^ A palette that endlessly repeats itself and exhaustively enumerates the possible /ISO colors/.
brightIsoColorPalette = cycle (ISOColor <$> [ 9 .. 14 ])

-- | Swap between the bright mode and the normal mode of the given 'Color'.
swapIsoColorBright
  :: Color  -- ^ The given 'Color' to convert to a more/less bright color.
  -> Color  -- ^ The corresponding variant that is more/less bright of the given 'Color'.
swapIsoColorBright (ISOColor i)
  | i < 8 = ISOColor (i+8)
  | otherwise = ISOColor (i-8)
swapIsoColorBright x = x

-- | Create the brighter color
isoColorToBright
  :: Color  -- ^ The given 'Color' to make /bright/.
  -> Color  -- ^ The corresponding /bright/ variant of the given 'Color'.
isoColorToBright (ISOColor i)
  | i < 8 = ISOColor (i+8)
isoColorToBright x = x

isoColorFromBright
  :: Color  -- ^ The given 'Color' to make a 'Color' that is less bright.
  -> Color  -- ^ The corresponding less bright variant of the given 'Color'.
isoColorFromBright (ISOColor i)
  | i >= 8 = ISOColor (i-8)
isoColorFromBright x = x

-- | Work with a color palette with 240 colors, but only 238 are selected, since black and white are not considered to be colors for the palette.
color240Palette
  :: Palette -- ^ A palette that endlessly repeats itself and exhaustively enumerates the possible /colors 240s/.
color240Palette = cycle (Color240 <$> take 238 (iterate ((1+). (`mod` 238) . (82+)) 1))
