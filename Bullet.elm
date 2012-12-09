
module Bullet (BulletPos, bulletsState, drawBullet) where

{- Section 1: Input -}

data BulletPos = BP (Float,Float) Float

-- getFiring :: TankPos -> TurretPos -> BulletPos
getFiring (T (x,y) a) (TrP theta) = BP (x,y) theta

data BulletInput = Fire BulletPos
                 | Tick Float

-- fireInput :: Signal () -> Signal TankPos -> Signal TurretPos
--           -> Signal BulletInput
fireInput click ta tu = sampleOn click (lift2 getFiring ta tu)

-- bulletInput :: Signal () -> Signal TankPos -> Signal TurretPos
--             -> Signal Float 
--             -> Signal BulletInput
bulletInput click tank turret tick = 
  merge (fireInput click tank turret) 
        (lift Tick tick)

{- Section 2: Model -}

bulletSpeed = 10

-- stepBullets :: BulletInput -> [BulletPos] -> [BulletPos]
stepBullets (Fire bp)    bps      = bp : bps
stepBullets (Tick delta) []       = []
stepBullets (Tick delta) (bp:bps) = 
  let (BP (x,y) theta) = bp
      newX             = x + bulletSpeed * (cos theta)
      newY             = y - bulletSpeed * (sin theta)
  in (BP (newX, newY) theta) : stepBullets (Tick delta) bps

-- defaultBullets :: [BulletPos]
defaultBullets = []

-- bulletsState :: Signal () -> Signal TankPos -> Signal TurretPos
--              -> Signal Time
--              -> Signal [BulletPos]
bulletsState click tank turret tick =
  foldp stepBullets defaultBullets (bulletInput click tank turret tick)

{- Section 3: View -}

-- drawBullet :: BulletPos -> Form
drawBullet (BP (x,y) a) =
  filled green (circle 3 (truncate x, truncate y))