
module Game where

import JavaScript
import Keyboard.Raw
import Window

desiredFPS = constant (castIntToJSNumber 30)
foreign export jsevent "desiredFPS"
  desiredFPS :: Signal JSNumber

foreign import jsevent "trigger" (castIntToJSNumber 0)
  jsTime :: Signal JSNumber

time = lift castJSNumberToFloat jsTime

delta = lift snd $ foldp (\t1 (t0,d) -> (t1, t1-t0)) (0,0) time
-- Modeling the controls of the tank chassis

data Drive = Forward | Reverse | Stop
data Turn = Left | Right | Straight

data KeyInput = KI Drive Turn
defaultKeyInput = KI Stop Straight

-- updateTurn :: Int -> Turn -> Turn
updateTurn key turn =
  let leftKey = 65
      rightKey = 68
  in case turn of
    Left     -> if key == leftKey  then Straight else Up
    Right    -> if key == rightKey then Straight else Down
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

input = sampleOn delta (lift2 I delta keyInput)

data TankPos = T (Float, Float) Float

data GameState = GS TankPos

defaultGame = GS (T (200,200) 90)

{- Section 2: Update -}





-- drawTank :: TankPos -> Form
drawTank (T (x,y) theta) = filled black (rect 20 30 (x,y))



-- move :: Turn -> Drive -> TankPos -> TankPos
move turn drive (T (x,y) theta) = 
  let newTheta = if turn == Straight
                 then theta
                 else if turn == Left
                 then theta + 1
                 else theta - 1
  in T (x,y) newTheta

-- tankPos :: Signal ((Int, Int), Int)


-- map :: (Int, Int) -> Element
map (w, h) = let t1 = drawTank $ T (300,300) 90
                 t2 = drawTank $ T (200,200) 90
             in collage w h [t1, t2]


-- main :: Element or Signal Element
main = lift map Window.dimensions