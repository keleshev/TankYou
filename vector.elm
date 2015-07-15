module Vector where

type alias Vector = { x: Float, y: Float }

infixl 7 *.
(*.) : Float -> Vector -> Vector
(*.) factor vector =
  { vector | x <- vector.x * factor
           , y <- vector.y * factor }

infixl 6 +.
(+.) : Vector -> Vector -> Vector
(+.) v w = { v | x <- v.x + w.x, y <- v.y + w.y }

infixl 6 -.
(-.) : Vector -> Vector -> Vector
(-.) v w = { v | x <- v.x - w.x, y <- v.y - w.y }

zero = { x=0, y=0 }
