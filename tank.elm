module Tank where

import Vector exposing (Vector)


type alias Angular = { position: Float, velocity: Float }

type alias Tank = {
  --tracks: { left: Track, right Track },
  position: Vector,
  velocity: Vector,
  angular: Angular,
  radius: Float,
  mass: Float
}

inertia object = 1 / 6 * (object.radius * 2) ^ 2 * object.mass

default = {
  position=Vector.zero,
  velocity=Vector.zero,
  angular={ position=0, velocity=0 },
  radius=30,
  mass=1 }
  --1/6 x size^2 x mass
  --1/6 * 60 ^ 2 * 1

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
