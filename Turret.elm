
module Turret (TurretPos, turretState, drawTurret) where

--import Tank (TurretPos)
--import Reticule (ReticulePos)

data TankPos = T (Float, Float) Float
data ReticulePos = RP (Int, Int)

data TurretPos = TrP Float

-- turretState :: Signal TankPos -> Signal ReticulePos
--             -> Signal TurretPos
turretState (T (tx,ty) a) (RP (rxi,ryi)) =
  let rx = toFloat rxi
      ry = toFloat ryi
      ratio = (ry-ty) / (rx-tx)
  in TrP $ atan ratio

-- turret :: Element
turret =
  let c = 10
      wh = 2 * c + 1
      lenPill = 5
      sidePill = 5
      pill = filled grey $ rect lenPill sidePill (c,c)
  in collage wh wh [ pill ]

-- drawTurret :: TankPos -> ReticulePos -> Form
drawTurret (T coord a) (TrP theta) = 
  rotate (theta / (2 * pi)) $ toForm coord turret