module Geometry.Interop.LinearAlgebra.Direction2d exposing (toVec2)

{-| Conversion functions for `Direction2d`.

@docs toVec2

-}

import Direction2d exposing (Direction2d)
import Math.Vector2 exposing (Vec2)


{-| Convert a `Direction2d` to a `Vec2`.

    Direction2d.toVec2 Direction2d.x
    --> vec2 1 0

-}
toVec2 : Direction2d coordinates -> Vec2
toVec2 direction =
    Math.Vector2.fromRecord (Direction2d.unwrap direction)
