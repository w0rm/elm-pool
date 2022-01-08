module Geometry.Interop.LinearAlgebra.Direction3d exposing (toVec3, toVec4)

{-| Conversion functions for `Direction3d`.

@docs toVec3, toVec4

-}

import Direction3d exposing (Direction3d)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| Convert a `Direction3d` to a `Vec3`.

    Direction3d.toVec3 Direction3d.y
    --> vec3 0 1 0

-}
toVec3 : Direction3d coordinates -> Vec3
toVec3 direction =
    Math.Vector3.fromRecord (Direction3d.unwrap direction)


{-| Convert a `Direction3d` to a `Vec4`. The resulting `Vec4` will have a W
component of 0 so that it [is not affected by translation](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/)
when performing matrix transformations.

    Direction3d.toVec4 Direction3d.y
    --> vec4 0 1 0 0

-}
toVec4 : Direction3d coordinates -> Vec4
toVec4 direction =
    let
        { x, y, z } =
            Direction3d.unwrap direction
    in
    Math.Vector4.vec4 x y z 0
