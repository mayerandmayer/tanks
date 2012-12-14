
module Reticule where

data ReticulePos = ReP (Int, Int)

-- reticuleState :: Signal (Int,Int) -> Signal ReticulePos
reticuleState mp = lift ReP mp

-- reticule :: Element
reticule =
  let c = 10
      t = 3
      wh = (2*c)+1
      hori = solid red $ segment (c-c, c) (c+c, c)
      vert = solid red $ segment (c, c-c) (c, c+c)
      circ = outlined red $ circle (c - t) (c, c)
  in collage wh wh [hori, vert, circ]

-- drawReticule :: ReticulePos -> Form
drawReticule (ReP coord) = toForm coord reticule
