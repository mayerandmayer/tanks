
module Turret where

--import Tank (TurretPos)
--import Reticule (ReticulePos)

data TurretPos = TuP Float

-- turretState ::  TankPos ->  ReticulePos ->  TurretPos
turretState (TaP (tx,ty) a) (ReP (rxi,ryi)) =
  let rx = toFloat rxi
      ry = toFloat ryi
      ratio = (ry-ty) / (rx-tx)
--  in TrP $ atan ratio
  in TuP $ atan2 (ry-ty) (rx-tx)

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

-- drawTurret :: TankPos -> TurretPos -> Form
drawTurret (TaP coord _) (TuP theta) = 
  rotate (theta / (2 * pi)) $ toForm coord turret