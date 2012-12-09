
-- rect :: Int -> Int -> (Int,Int) -> Shape

-- tank :: (Int,Int) -> (Int) -> Form
tank (x,y) (theta) = filled black (rect 20 30 (x,y))

-- main :: Element or Signal Element
main = collage 600 600 [tank (300,300) 90]
