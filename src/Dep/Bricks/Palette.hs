module Dep.Bricks.Palette (
    -- * Palettes
    Palette, isoColorPalette, color240Palette
  ) where

import Graphics.Vty.Attributes.Color(Color(ISOColor, Color240))

-- | A palette is an (endless) list of 'Color's.
type Palette = [Color]

-- | Work with a color palette of the six ISO colors.
isoColorPalette
  :: Palette  -- ^ A palette that endlessly repeats itself and exhaustively enumerates the possible /ISO colors/.
isoColorPalette = cycle (ISOColor <$> [1 .. 6 ])

-- | Work with a color palette with 240 colors, but only 238 are selected, since black and white are not considered to be colors for the palette.
color240Palette
  :: Palette -- ^ A palette that endlessly repeats itself and exhaustively enumerates the possible /colors 240s/.
color240Palette = cycle (Color240 <$> take 238 (iterate ((1+). (`mod` 238) . (82+)) 1))
