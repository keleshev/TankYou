module Vector where

type alias Vector = { x: Float, y: Float }


infixl 7 *.
(*.) : Float -> Vector -> Vector
(*.) factor vector =
  { vector | x <- vector.x * factor
           , y <- vector.y * factor }


infixl 7 /.
(/.) : Vector -> Float -> Vector
(/.) vector divisor =
  { vector | x <- vector.x / divisor
           , y <- vector.y / divisor }


infixl 6 +.
(+.) : Vector -> Vector -> Vector
(+.) v w = { v | x <- v.x + w.x, y <- v.y + w.y }


infixl 6 -.
(-.) : Vector -> Vector -> Vector
(-.) v w = { v | x <- v.x - w.x, y <- v.y - w.y }


zero = { x=0, y=0 }
unit = { x=1, y=1 }


sum : List Vector -> Vector
sum = List.foldr (+.) zero


magnitude : Vector -> Float
magnitude v = sqrt (v.x ^ 2 + v.y ^ 2)


rotate : Vector -> Float -> Vector
rotate vector angle =
  { vector | x <- vector.x * cos angle - vector.y * sin angle
           , y <- vector.x * sin angle + vector.y * cos angle }
