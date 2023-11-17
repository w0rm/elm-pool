module Camera exposing
    ( Camera
    , ScreenCoordinates
    , animate
    , camera3d
    , focusOn
    , initial
    , mouseOrbiting
    , mouseWheelZoom
    , orbitingPrecision
    , ray
    , zoomOut
    )

import Angle exposing (Angle)
import Animator exposing (Timeline)
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Length exposing (Meters)
import Physics.Coordinates exposing (WorldCoordinates)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d
import Time exposing (Posix)
import Vector2d
import Viewpoint3d


type ScreenCoordinates
    = ScreenCoordinates Never


type alias Camera =
    { zoom : Timeline Float -- also used for orbiting precision
    , azimuth : Angle -- also used for aiming (hit azimuth is relative to camera azimuth)
    , elevation : Timeline Angle
    , focalPoint : Timeline (Point3d Meters WorldCoordinates)
    }


initial : Camera
initial =
    { zoom = Animator.init 0.9
    , azimuth = Angle.degrees -25
    , elevation = Animator.init (Angle.degrees 30)
    , focalPoint = Animator.init Point3d.origin
    }


ray :
    Camera
    -> Rectangle2d Pixels ScreenCoordinates
    -> Point2d Pixels ScreenCoordinates
    -> Axis3d Meters WorldCoordinates
ray camera =
    Camera3d.ray (camera3d camera)


camera3d : Camera -> Camera3d Meters WorldCoordinates
camera3d { azimuth, elevation, zoom, focalPoint } =
    let
        distance =
            Animator.move zoom Animator.at
                |> Quantity.interpolateFrom (Length.meters 0.5) (Length.meters 5)
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = pointFromTimeline focalPoint
                , groundPlane = SketchPlane3d.xy
                , azimuth = azimuth
                , elevation = angleFromTimeline elevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


animate : Posix -> Camera -> Camera
animate time camera =
    { camera
        | elevation = Animator.updateTimeline time camera.elevation
        , zoom = Animator.updateTimeline time camera.zoom
        , focalPoint = Animator.updateTimeline time camera.focalPoint
    }


mouseWheelZoom : Float -> Camera -> Camera
mouseWheelZoom deltaY camera =
    let
        newZoom =
            clamp 0 1 (Animator.move camera.zoom Animator.at - deltaY * 0.002)
    in
    { camera | zoom = Animator.go Animator.immediately newZoom camera.zoom }


mouseOrbiting : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Camera -> Camera
mouseOrbiting originalPosition newPosition camera =
    let
        ( deltaX, deltaY ) =
            newPosition
                |> Vector2d.from originalPosition
                |> Vector2d.components

        radiansInPixels =
            orbitingPrecision camera

        newAzimuth =
            camera.azimuth
                |> Quantity.minus (Quantity.at radiansInPixels deltaX)
                |> Angle.normalize

        newElevation =
            angleFromTimeline camera.elevation
                |> Quantity.plus (Quantity.at radiansInPixels deltaY)
                |> Quantity.clamp (Angle.degrees 6) (Angle.degrees 90)
    in
    { camera
        | azimuth = newAzimuth
        , elevation = Animator.go Animator.immediately newElevation camera.elevation
    }


zoomOut : Camera -> Camera
zoomOut camera =
    { camera
        | zoom = Animator.go Animator.verySlowly 1 camera.zoom
        , elevation = Animator.go Animator.verySlowly (Angle.degrees 50) camera.elevation
    }


focusOn : Point3d Meters WorldCoordinates -> Camera -> Camera
focusOn focalPoint camera =
    { camera
        | focalPoint = Animator.go Animator.quickly focalPoint camera.focalPoint
    }


{-| Make orbiting precision depend on zoom level.
Controls how much radians correspond to the change in mouse offset.
-}
orbitingPrecision : Camera -> Quantity Float (Quantity.Rate Angle.Radians Pixels)
orbitingPrecision { zoom } =
    Quantity.rate
        (Angle.radians (0.2 + Animator.move zoom Animator.at / 0.8))
        (Pixels.pixels (180 / pi))


{-| Read the angle value from the timeline
-}
angleFromTimeline : Timeline Angle -> Angle
angleFromTimeline angleTimeline =
    Angle.radians (Animator.move angleTimeline (Angle.inRadians >> Animator.at))


{-| Read the point value from the timeline
-}
pointFromTimeline : Timeline (Point3d Meters WorldCoordinates) -> Point3d Meters WorldCoordinates
pointFromTimeline pointTimeline =
    Point3d.fromRecord Length.meters <|
        Animator.xyz pointTimeline
            (Point3d.toMeters
                >> (\p -> { x = Animator.at p.x, y = Animator.at p.y, z = Animator.at p.z })
            )
