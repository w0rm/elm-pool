module Viewpoint3d exposing
    ( Viewpoint3d
    , lookAt, orbit, orbitZ, isometricElevation, isometric
    , eyePoint, viewDirection, viewPlane, xDirection, yDirection
    )

{-| A `Viewpoint3d` represents the position and orientation of a camera in 3D.

@docs Viewpoint3d


# Constructors

@docs lookAt, orbit, orbitZ, isometricElevation, isometric


# Properties

@docs eyePoint, viewDirection, viewPlane, xDirection, yDirection

-}

import Angle exposing (Angle)
import Camera3d.Types as Types
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Math.Matrix4 exposing (Mat4)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Viewpoint3d units coordinates =
    Types.Viewpoint3d units coordinates


{-| Construct a `Viewpoint3d` at the given eye point looking towards the given
focal point, with the given global up direction (which will typically be
`Direction3d.positiveZ` or `Direction3d.positiveY`). For example, to construct a
viewpoint at the point (10, 0, 5) looking towards the origin:

    viewpoint =
        Viewpoint3d.lookAt
            { eyePoint = Point3d.meters 10 0 5
            , focalPoint = Point3d.origin
            , upDirection = Direction3d.positiveZ
            }

    Viewpoint3d.eyePoint viewpoint
    --> Point3d.meters 10 0 5

    Viewpoint3d.xDirection viewpoint
    --> Direction3d.positiveY

    Viewpoint3d.yDirection viewpoint
    --> Direction3d.xz (Angle.degrees 116.57)

    Viewpoint3d.viewDirection viewpoint
    --> Direction3d.xz (Angle.degrees -153.43)

That is likely all you need to know but if you are interested in the details and
corner cases, read on!

The view direction of the returned viewpoint will point from the eye point to
the focal point. The Y direction will be chosen to be as close to the global up
direction as possible (the viewpoint will not have any 'roll') and the X
direction will point to the right.

If the direction from the eye point to the focal point is parallel to the global
up direction (that is, the viewpoint represents looking straight up or straight
down) then the X and Y directions will be chosen arbitrarily.

If the given eye point and focal point are coincident (so that there is no well-
defined view direction), then the returned frame will have its Y direction set
to the global up direction and its X and view directions will be chosen
arbitrarily.

-}
lookAt : { focalPoint : Point3d units coordinates, eyePoint : Point3d units coordinates, upDirection : Direction3d coordinates } -> Viewpoint3d units coordinates
lookAt arguments =
    let
        zVector =
            Vector3d.from arguments.focalPoint arguments.eyePoint

        yVector =
            Direction3d.toVector arguments.upDirection

        xVector =
            yVector |> Vector3d.cross zVector
    in
    case Direction3d.orthonormalize zVector yVector xVector of
        Just ( normalizedZDirection, normalizedYDirection, normalizedXDirection ) ->
            Types.Viewpoint3d <|
                Frame3d.unsafe
                    { originPoint = arguments.eyePoint
                    , xDirection = normalizedXDirection
                    , yDirection = normalizedYDirection
                    , zDirection = normalizedZDirection
                    }

        Nothing ->
            case Vector3d.direction zVector of
                Just zDirection ->
                    -- The view vector must be parallel to the up direction,
                    -- since it is non-zero and therefore otherwise would have
                    -- resulted in a valid orthonormalization; therefore, choose
                    -- an arbitrary 'up' direction that is perpendicular to the
                    -- view direction
                    Types.Viewpoint3d <|
                        Frame3d.withZDirection zDirection arguments.eyePoint

                Nothing ->
                    -- The view vector is zero (the eye point and focal point
                    -- are coincident), so construct an arbitrary frame with the
                    -- given up direction
                    let
                        ( arbitraryZDirection, arbitraryXDirection ) =
                            Direction3d.perpendicularBasis arguments.upDirection
                    in
                    Types.Viewpoint3d <|
                        Frame3d.unsafe
                            { originPoint = arguments.eyePoint
                            , xDirection = arbitraryXDirection
                            , yDirection = arguments.upDirection
                            , zDirection = arbitraryZDirection
                            }


{-| Construct a `Viewpoint3d` looking at the given focal point, the given
distance away. The direction from the focal point to the eye point is defined by
the given azimuth and elevation angles, which are with respect to the given
ground plane (the position of the ground plane does not matter, only its
orientation). For example,

    Viewpoint3d.orbit
        { focalPoint = Point3d.meters 0 0 1
        , groundPlane = SketchPlane3d.xy
        , azimuth = Angle.degrees 0
        , elevation = Angle.degrees 45
        , distance = Length.meters 10
        }

is equivalent to

    Viewpoint3d.lookAt
        { focalPoint = Point3d.meters 0 0 1
        , eyePoint = Point3d.meters 7.071 0 8.071
        , upDirection = Direction3d.z
        }

As the name suggests, `Viewpoint3d.orbit` is useful for making orbiting cameras;
you can orbit around the focal point by changing just the azimuth, and rotate
up and down by changing just the elevation.

-}
orbit :
    { focalPoint : Point3d units coordinates
    , groundPlane : SketchPlane3d units coordinates defines
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float units
    }
    -> Viewpoint3d units coordinates
orbit arguments =
    let
        initialFrame =
            Frame3d.unsafe
                { originPoint = arguments.focalPoint
                , zDirection = SketchPlane3d.xDirection arguments.groundPlane
                , xDirection = SketchPlane3d.yDirection arguments.groundPlane
                , yDirection = SketchPlane3d.normalDirection arguments.groundPlane
                }

        finalFrame =
            initialFrame
                |> Frame3d.rotateAroundOwn Frame3d.yAxis arguments.azimuth
                |> Frame3d.rotateAroundOwn Frame3d.xAxis (Quantity.negate arguments.elevation)
                |> Frame3d.translateAlongOwn Frame3d.zAxis arguments.distance
    in
    Types.Viewpoint3d finalFrame


{-| A special case of `orbit` for orbiting around a Z axis through the given
focal point. This corresponds to setting `groundPlane` to `SketchPlane3d.xy`,
so azimuth is measured from the X axis towards the Y axis and elevation is
measured up from the XY plane. Not related to the [classic soft drink](https://en.wikipedia.org/wiki/Orbitz_%28drink%29).
-}
orbitZ :
    { focalPoint : Point3d units coordinates
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float units
    }
    -> Viewpoint3d units coordinates
orbitZ { focalPoint, azimuth, elevation, distance } =
    orbit
        { focalPoint = focalPoint
        , groundPlane = SketchPlane3d.xy
        , azimuth = azimuth
        , elevation = elevation
        , distance = distance
        }


{-| Not actually a constructor, but a useful value (approximately 35.26 degrees)
to use when constructing viewpoints using `orbit` or `orbitZ`: using this as the
`elevation` value will result in an [isometric](#isometric) viewpoint if
`azimuth` is set to 45 degrees. Said another way, this is the elevation angle of
a vector with components (1, 1, 1).
-}
isometricElevation : Angle
isometricElevation =
    Angle.atan2 (Quantity.float 1) (Quantity.float (sqrt 2))


{-| Construct a viewpoint looking at the given focal point, the given distance
away, such that a set of XYZ axes at that point will appear to have:

  - Z straight up
  - X pointing to the left and 30 degrees down
  - Y pointing to the right and 30 degrees down

You can combine `Viewpoint3d.isometric` with [`Camera3d.orthographic`](Camera3d#orthographic)
to achieve [isometric projection](https://en.wikipedia.org/wiki/Isometric_projection).

-}
isometric :
    { focalPoint : Point3d units coordinates
    , distance : Quantity Float units
    }
    -> Viewpoint3d units coordinates
isometric { focalPoint, distance } =
    orbitZ
        { focalPoint = focalPoint
        , azimuth = Angle.degrees 45
        , elevation = isometricElevation
        , distance = distance
        }


{-| Get the actual eye point of a viewpoint.
-}
eyePoint : Viewpoint3d units coordinates -> Point3d units coordinates
eyePoint (Types.Viewpoint3d frame) =
    Frame3d.originPoint frame


{-| Get the viewing direction of a viewpoint.
-}
viewDirection : Viewpoint3d units coordinates -> Direction3d coordinates
viewDirection (Types.Viewpoint3d frame) =
    Direction3d.reverse (Frame3d.zDirection frame)


{-| The view plane of a viewpoint is a `SketchPlane3d` perpendicular to the view
direction, with origin point equal to the eye point. For an observer looking
straight down the view direction, the X direction of the view plane points to
the right and the Y direction points up; this means that the view plane's normal
direction is the _opposite_ of the view direction. (Note that the Y direction
will _not_ be equal to the global up direction unless the view direction is
horizontal).
-}
viewPlane : Viewpoint3d units coordinates -> SketchPlane3d units coordinates defines
viewPlane (Types.Viewpoint3d frame) =
    Frame3d.xySketchPlane frame


{-| Get the X (right) direction of a viewpoint's view plane.
-}
xDirection : Viewpoint3d units coordinates -> Direction3d coordinates
xDirection (Types.Viewpoint3d frame) =
    Frame3d.xDirection frame


{-| Get the Y (local up) direction of a viewpoint's view plane.
-}
yDirection : Viewpoint3d units coordinates -> Direction3d coordinates
yDirection (Types.Viewpoint3d frame) =
    Frame3d.yDirection frame
