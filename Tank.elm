
module Tank where

{- Section 1: Input -}

data Drive = Forward | Reverse | Stop
data Turn = Left | Right | Straight

data TankInput = TI Drive Turn
defaultTankInput = TI Stop Straight

-- updateTurn :: Int -> Turn -> Turn
updateTurn key turn =
  let leftKey = 65
      rightKey = 68
  in case turn of
    Left     -> if key == leftKey  then Straight else Left
    Right    -> if key == rightKey then Straight else Right
    Straight -> if key == leftKey  then Left else
                if key == rightKey then Right else Straight

-- updateDrive :: Int -> Drive -> Drive
updateDrive key drive =
  let fwdKey = 87
      revKey = 83
  in case drive of
    Forward -> if key == fwdKey then Stop else Forward
    Reverse -> if key == revKey then Stop else Reverse
    Stop    -> if key == fwdKey then Forward else
               if key == revKey then Reverse else Stop

-- updateInput :: Int -> TankInput -> TankInput
updateTankInput key (TI drive turn) = 
  TI (updateDrive key drive) (updateTurn key turn)

-- tankInput :: Signal [Int] -> Signal TankInput
tankInput ks = lift (foldl updateTankInput defaultTankInput) ks

data SampledTankInput = STI Float TankInput

-- sampledTankInput :: Signal Time -> 
--                     Signal [Int] ->
--                     Signal SampledTankInput
sampledTankInput tick ks = 
  sampleOn tick (lift2 STI tick (tankInput ks))

{- Section 2: Model -}

data TankPos = T (Float, Float) Float
defaultTank = T (200,200) 0

turnRate = 0.002
driveRate = 4

-- stepTank :: SampledTankInput -> TankPos -> TankPos
stepTank (STI delta (TI drive turn)) (T (x,y) theta) =
  let newTheta = case turn of
                   Straight -> theta
                   Left -> theta + (turnRate * delta)
                   Right -> theta - (turnRate * delta)
      newX = case drive of
               Stop -> x
               Forward -> x + (driveRate * (cos newTheta))
               Reverse -> x - (driveRate * (cos newTheta))
      newY = case drive of
               Stop -> y
               Forward -> y - (driveRate * (sin newTheta))
               Reverse -> y + (driveRate * (sin newTheta))
  in T (newX, newY) newTheta

-- tankState :: Signal Time ->
--              Signal [Int] ->
--              Signal TankPos
tankState tick ks = 
  foldp stepTank defaultTank (sampledTankInput tick ks)

{- View -}

-- drawTank :: TankPos -> Form
drawTank (T (x,y) theta) = 
  let angle = 0 - (theta / (2 * pi)) in
  rotate angle $ filled black (rect 30 20 (truncate x, truncate y))
