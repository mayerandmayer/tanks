
module Game where

import JavaScript
import Keyboard.Raw
import Window

{-
desiredFPS = constant (castIntToJSNumber 30)
foreign export jsevent "desiredFPS"
  desiredFPS :: Signal JSNumber

foreign import jsevent "trigger" (castIntToJSNumber 0)
  jsTime :: Signal JSNumber

time = lift castJSNumberToFloat jsTime

delta = lift snd $ foldp (\t1 (t0,d) -> (t1, t1-t0)) (0,0) time
-- Modeling the controls of the tank chassis
-}

delta = fps 30

data Drive = Forward | Reverse | Stop
data Turn = Left | Right | Straight

data KeyInput = KI Drive Turn
defaultKeyInput = KI Stop Straight

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

-- updateInput :: Int -> KeyInput -> KeyInput
updateInput key (KI drive turn) = 
  KI (updateDrive key drive) (updateTurn key turn)

-- keyInput :: Signal KeyInput
keyInput = lift (foldl updateInput defaultKeyInput) keysDown

data Input = I Float KeyInput

-- input :: Signal Input
input = sampleOn delta (lift2 I delta keyInput)

data TankPos = T (Float, Float) Float

data GameState = GS TankPos
defaultGame = GS (T (200,200) 0)

{- Section 2: Update -}

turnRate = 0.005
driveRate = 10

-- stepTank :: Float -> KeyInput -> TankPos -> TankPos
stepTank delta (KI drive turn) (T (x,y) theta) =
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

-- stepGame :: Input -> GameState -> GameState
stepGame (I delta ki) (GS tank) = GS $ stepTank delta ki tank

-- gameState :: Signal GameState
gameState = foldp stepGame defaultGame input

-- drawTank :: TankPos -> Form
drawTank (T (x,y) theta) = 
  let angle = 0 - (theta / (2 * pi)) in
  rotate angle $ filled black (rect 30 20 (truncate x, truncate y))

-- map :: (Int, Int) -> GameState -> Element
map (w, h) (GS t) = collage w h [drawTank t]

-- main :: Element or Signal Element
main = lift2 map Window.dimensions gameState