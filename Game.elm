
module Game where

import Keyboard.Raw
import Mouse
import Time
import Window

import Tank (TankPos, tankState, drawTank)

import Reticule (ReticulePos, reticuleState, drawReticule)

import Turret (TurretPos, turretState, drawTurret)

{- Global Signals -}
tick = fps 30
-- keysDown
-- Mouse.position

data GameState = GS TankPos ReticulePos TurretPos

gTankState = tankState tick keysDown
gReticuleState = reticuleState Mouse.position
gTurretState = turretState gTankState gReticuleState

-- gameState :: Signal GameState
gameState = lift3 GS gTankState gReticuleState gTurretState

-- map :: (Int, Int) -> GameState -> Element
display (w, h) (GS ta re tu) = 
  collage w h [ drawTank ta
              , drawReticule re
              , drawTurret ta tu
              ]

-- main :: Signal Element
main = lift2 display Window.dimensions gameState
