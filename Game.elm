
module Game where

import Keyboard.Raw
import Mouse
import Time
import Window

import Tank (TankPos, tankState, drawTank)
import Reticule (ReticulePos, reticuleState, drawReticule)
import Turret (TurretPos, turretState, drawTurret)
import Bullet (BulletPos, bulletsState, drawBullet)

-- tick :: Signal Time
tick = fps 30

data GameState = GS TankPos ReticulePos TurretPos [BulletPos]

-- gameState :: Signal GameState
gameState = let gTankState = tankState tick keysDown
                gReticuleState = reticuleState Mouse.position
                gTurretState = lift2 turretState gTankState gReticuleState
                gBulletsState = bulletsState Mouse.clicks gTankState gTurretState tick
            in lift4 GS gTankState 
                        gReticuleState 
                        gTurretState
                        gBulletsState

-- map :: (Int, Int) -> GameState -> Element
display (w, h) (GS ta re tu bs) = 
  collage w h ( drawTank ta
              : drawReticule re
              : drawTurret ta tu
              : map drawBullet bs
              )

-- main :: Signal Element
main = lift2 display Window.dimensions gameState
