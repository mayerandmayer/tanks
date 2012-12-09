
module Turret (TurretPos, turretState, drawTurret) where

import Tank (TurretPos)
import Reticule (ReticulePos)

data TurretPos = TrP Float

-- turretState :: Signal TankPos -> Signal ReticulePos
--             -> Signal TurretPos
turretState (T (tx,ty) _) (RP (rxi,ryi)) =
  let rx = toFloat rxi
      ry = toFloat ryi
      ratio = (ry-ty) / (rx-tx)
  in TrP $ atan ratio

-- turret :: Element
turret =
  let c = 10
      wh = 2 * c + 1
      lenPill = 5
      widthPill = 5
      pill = filled grey $ rect wp hp (c,c)
  in collage wh wh [ pill ]

-- drawTurret :: TankPos -> ReticulePos -> Form
drawTurret (T coord _) (TrP theta) = 
  rotate (theta / (2 * pi)) $ toForm coord turret