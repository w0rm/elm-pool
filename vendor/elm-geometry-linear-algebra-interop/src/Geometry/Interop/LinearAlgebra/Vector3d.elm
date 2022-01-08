module Geometry.Interop.LinearAlgebra.Vector3d exposing (toVec3, toVec4, fromVec3, transformBy)

{-| Conversion and transformation functions for `Vector3d`.

@docs toVec3, toVec4, fromVec3, transformBy

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Vector3d exposing (Vector3d)


{-| Convert a `Vector3d` to a `Vec3`.

    Vector3d.toVec3 (Vector3d.meters 2 1 3)
    --> Vector3.vec3 2 1 3

-}
toVec3 : Vector3d units coordinates -> Vec3
toVec3 vector =
    Math.Vector3.fromRecord (Vector3d.unwrap vector)


{-| Convert a `Vector3d` to a `Vec4`. The resulting `Vec4` will have a W
component of 0 so that it [is not affected by translation](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/)
when performing matrix transformations.

    Vector3d.toVec4 (Vector3d.meters 2 1 3)
    --> vec4 2 1 3 0

-}
toVec4 : Vector3d units coordinates -> Vec4
toVec4 vector =
    let
        { x, y, z } =
            Vector3d.unwrap vector
    in
    Math.Vector4.vec4 x y z 0


{-| Convert a `Vec3` to a `Vector3d`.

    Vector3d.fromVec3 (Vector3.vec3 2 1 3)
    --> Vector3d.unsafe { x = 2, y = 1, z = 3 }

-}
fromVec3 : Vec3 -> Vector3d units coordinates
fromVec3 vec =
    Vector3d.unsafe (Math.Vector3.toRecord vec)


{-| Transform a `Vector3d` by a `Mat4`; note that

    vector
        |> Vector3d.transformBy matrix

is similar to but _not_ in general equivalent to

    vector
        |> Vector3d.toVec3
        |> Matrix4.transform matrix
        |> Vector3d.fromVec3

since `Matrix4.transform` implicitly assumes that the given argument represents
a point, not a vector, and therefore applies translation to it. Transforming a
vector by a 4x4 matrix should in fact ignore any translation component of the
matrix, which this function does. For example:

    vector =
        Vector3d.meters 2 1 3

    -- 90 degree rotation around the Z axis,
    -- followed by a translation
    matrix =
        Matrix4.makeTranslate3 5 5 5
            |> Matrix4.rotate (degrees 90) Vector3.k

    Vector3d.transformBy matrix vector
    --> Vector3d.meters -1 2 3

-}
transformBy : Mat4 -> Vector3d units1 coordinates1 -> Vector3d units2 coordinates2
transformBy matrix vector =
    let
        { m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44 } =
            Math.Matrix4.toRecord matrix

        { x, y, z } =
            Vector3d.unwrap vector
    in
    Vector3d.unsafe
        { x = m11 * x + m12 * y + m13 * z
        , y = m21 * x + m22 * y + m23 * z
        , z = m31 * x + m32 * y + m33 * z
        }
