import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

import Tank exposing (Tank)
import Vector exposing (Vector, (+.), (*.))
import Controls



-- UPDATE --
jump {left} ({velocity} as tank) =
  if left > 0 && tank.position.y == 0 then
    { tank | velocity <- { velocity | y <- 5 }}
  else tank

{-gravity dt ({position, velocity} as tank) =
  if position.y > 0 then
    { tank | velocity <- { velocity | y <- velocity.y - dt / 4 }}
  else tank
  -}

physics dt ({position, velocity} as tank) =
  { tank | position <- position +. dt *. velocity }

walk {right} ({velocity} as tank) =
  { tank | velocity <- { velocity | x <- toFloat right }}

update (dt, {tracks}) tank =
  tank |> jump tracks |> walk tracks |> physics dt



-- VIEW
view (w',h') tank =
  let (w,h) = (toFloat w', toFloat h') in
    collage w' h'
      [ rect w h  |> filled Color.white
      , square tank.radius |> filled Color.black
                           |> move (tank.position.x, tank.position.y)
                           |> rotate tank.angular.position
      ]


-- SIGNALING
input = let delta = Signal.map (\t -> t/20) (fps 60)
        in  Signal.sampleOn delta (Signal.map2 (,) delta Controls.all)

main =
  Signal.map2 view Window.dimensions (Signal.foldp update Tank.default input)

