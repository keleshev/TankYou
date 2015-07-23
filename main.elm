import Time exposing (fps)
import Window
import Debug

import Tank exposing (Tank)
import Vector exposing (Vector, (+.), (*.), (/.), (×))
import Controls exposing (Controls)
import View



-- UPDATE --

update : (Float, Controls) -> Tank -> Tank
update (dt', controls) tank =
  let _ = 1 / dt' |> floor |> Debug.watch "fps" in
  let dt = min dt (1 / 10) in
  let _ = controls |> Debug.watch "controls" in
  let forces = inferForces tank controls in
  let force = sumForces forces in
  let torque = inferTorque forces in
  let torque' = torque - 600 * tank.angular.velocity in
  let torque'' = torque' - 50 * tank.angular.velocity ^ 2 in
  let tank' = applyForce tank force dt in
  let tank'' = applyTorque tank' torque'' dt in
  tank'' |> Debug.watch "tank"


type alias Force = Vector
type alias AppliedForce = { force: Force, point: Vector }


sign x = if x > 0 then 1 else if x < 0 then -1 else 0


appliedForceToTorque : AppliedForce -> Float
appliedForceToTorque {force, point} = force × point


inferTorque : List AppliedForce -> Float
inferTorque appliedForces =
  appliedForces |> List.map appliedForceToTorque |> List.sum


throttleToForce throttle = (toFloat throttle) * 15.0


inferForces : Tank -> Controls -> List AppliedForce
inferForces tank {tracks} =
  let angle = tank.angular.position in
  [ { force=Vector.rotate { x=0, y=throttleToForce tracks.right } angle,
      point=Vector.rotate { x=(-1 * tank.radius), y=0 } angle },
    { force=Vector.rotate { x=0, y=throttleToForce tracks.left } angle,
      point=Vector.rotate { x=(1 * tank.radius), y=0 } angle },
    { force=dragForce tank.velocity, point=Vector.zero },
    { force=frictionForce tank, point=Vector.zero }
  ]


frictionForce : Tank -> Force
frictionForce {velocity, angular} =
  let angle = angular.position in
  let velocityAtTank = Vector.rotate velocity -angle in
  let k = {x=1.00 , y=0.1} in
  let friction = {velocityAtTank | x <- velocityAtTank.x * -k.x,
                                   y <- velocityAtTank.y * -k.y} in
  Vector.rotate friction angle


dragForce : Vector -> Force
dragForce velocity =
  let dragCoefficient = 0.005 in
  -1 * dragCoefficient * (Vector.magnitude velocity) *. velocity


sumForces : List AppliedForce -> Force
sumForces appliedForces =
  appliedForces |> List.map .force |> Vector.sum


applyForce : Tank -> Force -> Float -> Tank
applyForce tank force dt =
  let acceleration = force /. tank.mass in
  let velocity = tank.velocity +. dt *. acceleration in
  --let old_momentum = tank.mass *. tank.velocity in
  --let momentum = old_momentum +. dt *. force in
  --let velocity = momentum /. tank.mass in

  let position = tank.position +. dt *. velocity in
  { tank | velocity <- velocity,
           position <- position }


applyTorque : Tank -> Float -> Float -> Tank
applyTorque ({angular} as tank) torque dt =
  let angular_acceleration = torque / (Tank.inertia tank) in
  let angular_velocity = angular.velocity + dt * angular_acceleration in
  let angular_position = angular.position + dt * angular_velocity in
  { tank | angular <- { angular | position <- angular_position,
                                  velocity <- angular_velocity } }



-- SIGNALING
input =
  let delta = Signal.map (\t -> t / 1000) (fps 60) in
  Signal.sampleOn delta (Signal.map2 (,) delta Controls.all)

main =
  let signal = Signal.foldp update Tank.default input in
  Signal.map2 View.render Window.dimensions signal
