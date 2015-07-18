module View where

import Color
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (
  Form, collage, circle, rect, square, filled, move, rotate, group)
import Debug

import Tank exposing (Tank)


render : (Int, Int) -> Tank -> Element
render (w', h') tank =
  let (w, h) = (toFloat w', toFloat h') in
  collage w' h' [ rect w h |> filled Color.white, renderTank tank ]

black = filled Color.black
white = filled Color.white
grey = filled Color.lightCharcoal


renderTank : Tank -> Form
renderTank tank =
  let radius = tank.radius  in
  let diameter = radius * 2 in
  let track = rect (radius * 0.75) (diameter * 1.3) |> grey in
  group [ --square tank.radius |> grey,
          track |> move (radius, 0),
          track |> move (-radius, 0),
          rect 6 diameter |> black |> move (0, diameter * 0.5),
          circle radius |> black ]
    |> move (tank.position.x, tank.position.y)
    |> rotate tank.angular.position
