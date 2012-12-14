
module Bullet where

{- Section 1: Input -}

data BulletPos = BuP (Float,Float) Float

-- getFiring :: TankPos -> TurretPos -> BulletPos
getFiring (TaP (x,y) a) (TuP theta) = BuP (x,y) theta

data BulletInput = Fire BulletPos
                 | Tick Float

-- bulletInput :: Signal () -> Signal TankPos -> Signal TurretPos
--             -> Signal Float 
--             -> Signal BulletInput
bulletInput click ta tu tick = 
  let fireInput = sampleOn click (lift2 getFiring ta tu)
  in merge (lift Fire fireInput) 
           (lift Tick tick)

{- Section 2: Model -}

bulletSpeed = 10

--moveBullet :: Float -> BulletPos -> BulletPos
moveBullet delta (BuP (x,y) theta) = 
  let newX = x + (bulletSpeed * (cos theta))
      newY = y + (bulletSpeed * (sin theta))
  in BuP (newX, newY) theta

-- stepBullets :: BulletInput -> [BulletPos] -> [BulletPos]
stepBullets bi bps = 
  case bi of
    (Fire bp)    -> bp : bps
    (Tick delta) -> map (moveBullet delta) bps

-- defaultBullets :: [BulletPos]
defaultBullets = []

-- bulletsState :: Signal () -> Signal TankPos -> Signal TurretPos
--              -> Signal Time
--              -> Signal [BulletPos]
bulletsState click tank turret tick =
  foldp stepBullets defaultBullets (bulletInput click tank turret tick)

{- Section 3: View -}

-- drawBullet :: BulletPos -> Form
drawBullet (BuP (x,y) a) =
  filled green (circle 3 (truncate x, truncate y))