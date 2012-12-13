
module ShootDemo where

import Mouse
import Time
import Window

import Tank 
import Turret
import Bullet 

{- Global Signals -}
tick = fps 30
-- keysDown
-- Mouse.position
-- Mouse.clicks

data GameState = GS [BulletPos]

-- gameState :: Signal GameState
gameState = let gTankState = constant $ T (200,200) 0
                gTurretState = constant $ TrP 0
                gBulletsState = bulletsState Mouse.clicks gTankState gTurretState tick
            in lift GS gBulletsState

-- map :: (Int, Int) -> GameState -> Element
display (w, h) (GS bs) = 
  collage w h $ map drawBullet bs

-- main :: Signal Element
main = lift2 display Window.dimensions gameState

