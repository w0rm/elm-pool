module Triangle3d.Projection exposing (toScreenSpace, isFrontFacing)

{-|

@docs toScreenSpace, isFrontFacing

-}

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Frame3d
import LineSegment3d
import Plane3d
import Point3d
import Point3d.Projection as Point3d
import Quantity exposing (zero)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector3d
import Viewpoint3d


{-| Project a triangle from 3D world to 2D screen coordinates. Equivalent
to calling [`Point3d.toScreenSpace`](Point3d-Projection#toScreenSpace) on the
three vertices of the triangle.
-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Triangle3d worldUnits worldCoordinates
    -> Triangle2d screenUnits screenCoordinates
toScreenSpace camera screen triangle =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
    Triangle2d.from
        (Point3d.toScreenSpace camera screen p1)
        (Point3d.toScreenSpace camera screen p2)
        (Point3d.toScreenSpace camera screen p3)


{-| Check if a given triangle is front facing with respect to the given camera.
A 'front facing' triangle has a normal direction towards the camera instead of
away. This also means that when projected into 2D, the vertices of the triangle
will be in counterclockwise order.
-}
isFrontFacing : Camera3d units coordinates -> Triangle3d units coordinates -> Bool
isFrontFacing camera triangle =
    case Triangle3d.normalDirection triangle of
        Just normalDirection ->
            let
                viewpoint =
                    Camera3d.viewpoint camera

                eyePoint =
                    Viewpoint3d.eyePoint viewpoint

                ( p1, _, _ ) =
                    Triangle3d.vertices triangle
            in
            Vector3d.from p1 eyePoint
                |> Vector3d.componentIn normalDirection
                |> Quantity.greaterThan zero

        Nothing ->
            False
