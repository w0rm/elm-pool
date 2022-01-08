module Geometry.Interop.LinearAlgebra.Frame3d exposing (toMat4)

{-| Conversion functions for `Frame3d`.

@docs toMat4

-}

import Direction3d
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
import Point3d


{-| Convert a `Frame3d` to a `Mat4`. The resulting matrix can be thought of in
a couple of ways:

  - It is the transformation matrix that takes the global XYZ frame and
    transforms it to the given frame
  - It is the transformation matrix from local coordinates in the given frame
    to global coordinates

The first bullet implies that something like

    Frame3d.atOrigin
        |> Frame3d.translateBy displacement
        |> Frame3d.rotateAround axis angle
        |> Frame3d.mirrorAcross plane
        |> Frame3d.toMat4

gives you a transformation matrix that is equivalent to applying the given
displacement, then the given rotation, then the given mirror. The second bullet
means that, for example,

    Point3d.placeIn frame

is equivalent to

    Point3d.transformBy (Frame3d.toMat4 frame)

and

    Point3d.relativeTo frame

is equivalent to

    Point3d.transformBy <|
        Matrix4.inverseOrthonormal
            (Frame3d.toMat4 frame)

-}
toMat4 : Frame3d units coordinates defines -> Mat4
toMat4 frame =
    let
        i =
            Direction3d.unwrap (Frame3d.xDirection frame)

        j =
            Direction3d.unwrap (Frame3d.yDirection frame)

        k =
            Direction3d.unwrap (Frame3d.zDirection frame)

        p =
            Point3d.unwrap (Frame3d.originPoint frame)
    in
    Math.Matrix4.fromRecord
        { m11 = i.x
        , m21 = i.y
        , m31 = i.z
        , m41 = 0
        , m12 = j.x
        , m22 = j.y
        , m32 = j.z
        , m42 = 0
        , m13 = k.x
        , m23 = k.y
        , m33 = k.z
        , m43 = 0
        , m14 = p.x
        , m24 = p.y
        , m34 = p.z
        , m44 = 1
        }
