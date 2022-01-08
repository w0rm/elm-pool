module Camera3d exposing
    ( Camera3d
    , perspective, orthographic
    , viewpoint
    , ray
    )

{-| A `Camera3d` is a perspective or orthographic camera in 3D, encapsulating
the camera's viewpoint and projection matrix as well as the dimensions of the
screen the camera renders to. This module contains functions for:

  - Defining perspective and orthographic cameras
  - Obtaining view and projection matrices for a given camera, which can then be
    used for WebGL rendering
  - Using cameras to project 3D geometry to 2D screen space

@docs Camera3d


# Constructors

@docs perspective, orthographic


# Properties

@docs viewpoint


# Ray casting

@docs ray

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Camera3d.Types as Types
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Math.Matrix4 exposing (Mat4)
import Math.Vector4 exposing (Vec4)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), zero)
import Rectangle2d exposing (Rectangle2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)


{-| -}
type alias Camera3d units coordinates =
    Types.Camera3d units coordinates


{-| Create a perspective camera from a viewpoint and a vertical field of view.

    perspectiveCamera =
        Camera3d.perspective
            { viewpoint = cameraViewpoint
            , verticalFieldOfView = Angle.degrees 30
            }

-}
perspective :
    { viewpoint : Viewpoint3d units coordinates
    , verticalFieldOfView : Angle
    }
    -> Camera3d units coordinates
perspective arguments =
    let
        halfFieldOfView =
            Quantity.half (Quantity.abs arguments.verticalFieldOfView)

        frustumSlope =
            Angle.tan halfFieldOfView
    in
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , projection = Types.Perspective frustumSlope
        }


{-| Create an orthographic camera from a viewpoint and the height of the
orthographic viewport: this is the height in 3D world units of the section of
the model to be rendered.

    orthographicCamera =
        Camera3d.orthographic
            { viewpoint = cameraViewpoint
            , viewportHeight = Length.meters 5
            }

-}
orthographic :
    { viewpoint : Viewpoint3d units coordinates
    , viewportHeight : Quantity Float units
    }
    -> Camera3d units coordinates
orthographic arguments =
    Types.Camera3d
        { viewpoint = arguments.viewpoint
        , projection = Types.Orthographic (Quantity.abs arguments.viewportHeight)
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera3d units coordinates -> Viewpoint3d units coordinates
viewpoint (Types.Camera3d camera) =
    camera.viewpoint


{-| Given a camera, a rectangle defining the shape and size of a screen, and a
2D point within that screen, calculate the corresponding 3D ray as an `Axis3d`.
Conceptually, the ray will pass through the given point on the screen and will
have direction equal to the viewing direction at that point.

For a perspective camera, the origin of the ray will be constant (always equal
to the camera's eye point) and the direction will vary depending on the 2D
screen point. For an orthographic camera, the direction of the ray will be
constant (the view direction of the camera) but the origin will vary depending
on the 2D screen point.

-}
ray :
    Camera3d units coordinates
    -> Rectangle2d screenUnits screenCoordinates
    -> Point2d screenUnits screenCoordinates
    -> Axis3d units coordinates
ray (Types.Camera3d camera) screen point =
    let
        (Types.Viewpoint3d viewpointFrame) =
            camera.viewpoint

        ( screenWidth, screenHeight ) =
            Rectangle2d.dimensions screen

        screenX =
            Point2d.xCoordinateIn (Rectangle2d.axes screen) point

        screenY =
            Point2d.yCoordinateIn (Rectangle2d.axes screen) point
    in
    case camera.projection of
        Types.Perspective frustumSlope ->
            let
                screenZ =
                    Quantity.multiplyBy 0.5 screenHeight
                        |> Quantity.divideBy frustumSlope
                        |> Quantity.negate

                direction =
                    Vector3d.xyz screenX screenY screenZ
                        |> Vector3d.direction
                        |> Maybe.withDefault Direction3d.negativeZ
                        |> Direction3d.placeIn viewpointFrame
            in
            Axis3d.through (Viewpoint3d.eyePoint camera.viewpoint) direction

        Types.Orthographic viewpointHeight ->
            let
                resolution =
                    viewpointHeight |> Quantity.per screenHeight

                origin =
                    Point3d.xyzIn viewpointFrame
                        (screenX |> Quantity.at resolution)
                        (screenY |> Quantity.at resolution)
                        zero
            in
            Axis3d.through origin (Viewpoint3d.viewDirection camera.viewpoint)
