import Time exposing (..)
import Window
import Debug

import Tank exposing (Tank)
import Vector exposing (Vector, (+.), (*.), (/.))
import Controls exposing (Controls)
import View



-- UPDATE --

update : (Float, Controls) -> Tank -> Tank
update (dt, controls) tank =
  let _ = controls |> Debug.watch "controls" in
  let forces = inferForces tank controls |> Debug.watch "forces" in
  let force = sumForces forces in
  let torque = inferTorque forces in
  let torque' = torque - 1000 * tank.angular.velocity in
  let tank' = applyForce tank force dt in
  let tank'' = applyTorque tank' torque' dt in
  tank'' |> Debug.watch "tank"


type alias Force = Vector
type alias AppliedForce = { force: Force, position: Vector }


sign x = if x > 0 then 1 else if x < 0 then -1 else 0


appliedForceToTorque : AppliedForce -> Float
appliedForceToTorque {force, position} =
  (sign position.x) * Vector.magnitude force * Vector.magnitude position

inferTorque : List AppliedForce -> Float
inferTorque appliedForces =
  appliedForces |> List.map appliedForceToTorque |> List.sum


throttleToForce throttle = (toFloat throttle) * 1


inferForces : Tank -> Controls -> List AppliedForce
inferForces tank {tracks} =
  let angle = tank.angular.position in
  [ { force=Vector.rotate { x=0, y=throttleToForce tracks.left } angle ,
      position={ x=(-2 * tank.radius * sign tracks.left), y=0 } },
    { force=Vector.rotate { x=0, y=throttleToForce tracks.right } angle ,
      position={ x=(2 * tank.radius * sign tracks.right), y=0 }},
    { force=dragForce tank.velocity, position=Vector.zero },
    { force=frictionForce tank.velocity, position=Vector.zero }
  ]


frictionForce : Vector -> Force
frictionForce velocity =
  let frictionCoefficient = 0.1 in
  -1 * frictionCoefficient *. velocity


dragForce : Vector -> Force
dragForce velocity =
  let dragCoefficient = 0.05 in
  -1 * dragCoefficient * (Vector.magnitude velocity) *. velocity


sumForces : List AppliedForce -> Force
sumForces appliedForces =
  appliedForces |> List.map .force |> Vector.sum


applyForce : Tank -> Force -> Float -> Tank
applyForce tank force dt =
  let acceleration = force /. tank.mass in
  let velocity = tank.velocity +. dt *. acceleration in
  let position = tank.position +. dt *. velocity in
  { tank | velocity <- velocity,
           position <- position }


applyTorque : Tank -> Float -> Float -> Tank
applyTorque ({angular} as tank) torque dt =
  let angular_acceleration = torque / tank.inertia in
  let angular_velocity = angular.velocity + dt * angular_acceleration in
  let angular_position = angular.position + dt * angular_velocity in
  { tank | angular <- { angular | position <- angular_position,
                                  velocity <- angular_velocity } }



-- SIGNALING
input =
  let delta = Signal.map (\t -> t/20) (fps 60) in
  Signal.sampleOn delta (Signal.map2 (,) delta Controls.all)

main =
  let signal = Signal.foldp update Tank.default input in
  Signal.map2 View.render Window.dimensions signal
