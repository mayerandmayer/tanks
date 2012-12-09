
module Turret (TurretPos, turretState, drawTurret) where

--import Tank (TurretPos)
--import Reticule (ReticulePos)

data TurretPos = TrP Float

-- turretState ::  TankPos ->  ReticulePos ->  TurretPos
turretState (T (tx,ty) a) (RP (rxi,ryi)) =
  let rx = toFloat rxi
      ry = toFloat ryi
      ratio = (ry-ty) / (rx-tx)
--  in TrP $ atan ratio
  in TrP $ atan2 (ry-ty) (rx-tx)

-- turret :: Element
turret =
  let c = 20
      wh = 2 * c + 1
      len = 12
      side = 8
      gauge = 2
      pill = filled grey $ rect len side (c,c)
      barrel = filled grey $ rect (2 * len) gauge (c+len, c)
      
  in collage wh wh [ pill, barrel ]

-- drawTurret :: TurretPos -> Form
drawTurret (T coord _) (TrP theta) = 
  rotate (theta / (2 * pi)) $ toForm coord turret