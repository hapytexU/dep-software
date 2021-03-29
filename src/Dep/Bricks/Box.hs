module Dep.Bricks.Box (
    boxh,  boxv
  , boxlb, boxlt, boxrb, boxrt
  , boxhu, boxhd, boxvl, boxvr
  , lineh, linev, negator
  ) where

linev :: Char
linev = '\x2502'

lineh :: Char
lineh = '\x2500'

boxlt :: Char
boxlt = '\x250f'

boxh :: Char
boxh = '\x2501'

boxv :: Char
boxv = '\x2503'

boxrt :: Char
boxrt = '\x2513'

boxlb :: Char
boxlb = '\x2517'

boxrb :: Char
boxrb = '\x251b'

boxhu :: Char
boxhu = '\x2537'

boxhd :: Char
boxhd = '\x252f'

boxvl :: Char
boxvl = '\x2528'

boxvr :: Char
boxvr = '\x2520'

negator :: Char
negator = 'O'
