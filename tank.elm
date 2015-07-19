module Tank where

import Vector exposing (Vector)


type alias Angular = { position: Float, velocity: Float }

type alias Tank = {
  --tracks: { left: Track, right Track },
  position: Vector,
  velocity: Vector,
  angular: Angular,
  radius: Float,
  mass: Float,
  inertia: Float
}

default = {
  position=Vector.zero,
  velocity=Vector.zero,
  angular={ position=0, velocity=0 },
  radius=30,
  mass=1,
  inertia=4000 }

{-
τ = 1

A = F / M
V = V + A * T
P = P + V * T


st.foldr (+.) Vector.zeroacceleration_ = force_ / mass
velocity_ = velocity_ + acceleration_ * t
position_ = position_ + velocity_ * t

AngA = Torque / Inertia
AngV = AngV + AngA * T
Angle = Angle + AngV * T

angular_acceleration = torque / inertia
angular_velocity = angular_velocity + angular_acceleration * t
angular_position = angular_position + angular_velocity * t





--infixr 9 *         -}
