module Point3d.Projection exposing (depth, toScreenSpace)

{-|

@docs depth, toScreenSpace

-}

import Camera3d exposing (Camera3d)
import Camera3d.Types as Types
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)


{-| Find the 'depth' of a point relative to a camera. This is the signed
distance of the point from a plane with:

  - origin point equal to the camera's eye point
  - normal direction equal to the camera's viewing direction

The returned depth is positive for points in front of the camera and negative
for points behind.

-}
depth : Camera3d units coordinates -> Point3d units coordinates -> Quantity Float units
depth (Types.Camera3d camera) point =
    let
        (Types.Viewpoint3d viewpointFrame) =
            camera.viewpoint
    in
    Quantity.negate (Point3d.zCoordinateIn viewpointFrame point)


{-| Project a point from 3D world to 2D screen coordinates, by supplying a
rectangle that defines the shape and size of the screen. Points directly ahead
of the camera will end up in the center of the given screen rectangle.

Note that the projected 2D point may be outside the given rectangle, if the
given 3D point is outside the field of view of the given camera.

-}
toScreenSpace :
    Camera3d worldUnits worldCoordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point3d worldUnits worldCoordinates
    -> Point2d screenUnits screenCoordinates
toScreenSpace (Types.Camera3d camera) screen point =
    let
        (Types.Viewpoint3d viewpointFrame) =
            camera.viewpoint

        viewX =
            Point3d.xCoordinateIn viewpointFrame point

        viewY =
            Point3d.yCoordinateIn viewpointFrame point

        viewZ =
            Point3d.zCoordinateIn viewpointFrame point

        pointDepth =
            Quantity.negate viewZ

        ( screenWidth, screenHeight ) =
            Rectangle2d.dimensions screen

        aspectRatio =
            Quantity.ratio screenWidth screenHeight
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            let
                ndcY =
                    Quantity.ratio viewY pointDepth / frustumSlope

                ndcX =
                    Quantity.ratio viewX pointDepth
                        / (aspectRatio * frustumSlope)
            in
            Point2d.xyIn (Rectangle2d.axes screen)
                (Quantity.multiplyBy (ndcX / 2) screenWidth)
                (Quantity.multiplyBy (ndcY / 2) screenHeight)

        Types.Orthographic viewportHeight ->
            let
                halfNdcY =
                    Quantity.ratio viewY viewportHeight

                halfNdcX =
                    Quantity.ratio viewX
                        (Quantity.multiplyBy aspectRatio viewportHeight)
            in
            Point2d.xyIn (Rectangle2d.axes screen)
                (Quantity.multiplyBy halfNdcX screenWidth)
                (Quantity.multiplyBy halfNdcY screenHeight)
