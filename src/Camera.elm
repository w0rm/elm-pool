module Camera exposing
    ( ScreenCoordinates, Camera, initial
    , camera3d, azimuth, orbitingPrecision
    , ray, mouseOrbiting, mouseWheelZoom
    , focusOn, zoomOut, animate
    )

{-| Animated 3d camera controls

@docs ScreenCoordinates, Camera, initial


# Current state

@docs camera3d, azimuth, orbitingPrecision


# Interaction

@docs ray, mouseOrbiting, mouseWheelZoom


# Animation

@docs focusOn, zoomOut, animate

-}

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


{-| Screen space coordinate system
-}
type ScreenCoordinates
    = ScreenCoordinates Never


{-| -}
type Camera
    = Camera
        { zoom : Timeline Float -- also used for orbiting precision
        , azimuth : Angle -- also used for aiming (hit azimuth is relative to camera azimuth)
        , elevation : Timeline Angle
        , focalPoint : Timeline (Point3d Meters WorldCoordinates)
        }


{-| Initial look at the table
-}
initial : Camera
initial =
    Camera
        { zoom = Animator.init 0.9
        , azimuth = Angle.degrees -115
        , elevation = Animator.init (Angle.degrees 30)
        , focalPoint = Animator.init Point3d.origin
        }



-- CURRENT STATE


{-| Get the currrent Camera3d for rendering with elm-3d-scene
-}
camera3d : Camera -> Camera3d Meters WorldCoordinates
camera3d (Camera camera) =
    let
        distance =
            Animator.move camera.zoom Animator.at
                |> Quantity.interpolateFrom (Length.meters 0.5) (Length.meters 6)

        focalPoint =
            Point3d.fromRecord Length.meters <|
                Animator.xyz camera.focalPoint
                    (Point3d.toMeters
                        >> (\p -> { x = Animator.at p.x, y = Animator.at p.y, z = Animator.at p.z })
                    )
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = focalPoint
                , groundPlane = SketchPlane3d.xy
                , azimuth = camera.azimuth
                , elevation = angleFromTimeline camera.elevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


{-| Get the currrent azimuth used for aiming the cue
-}
azimuth : Camera -> Angle
azimuth (Camera camera) =
    camera.azimuth


{-| Make orbiting precision depend on zoom level.
Controls how much radians correspond to the change in mouse offset.
-}
orbitingPrecision : Camera -> Quantity Float (Quantity.Rate Angle.Radians Pixels)
orbitingPrecision (Camera camera) =
    Quantity.rate
        (Angle.radians (0.2 + Animator.move camera.zoom Animator.at / 0.8))
        (Pixels.pixels (180 / pi))



-- INTERACTION


{-| Get the ray from the camera into the viewplane,
useful for mouse interactions with the 3d objects
-}
ray :
    Camera
    -> Rectangle2d Pixels ScreenCoordinates
    -> Point2d Pixels ScreenCoordinates
    -> Axis3d Meters WorldCoordinates
ray camera =
    Camera3d.ray (camera3d camera)


{-| Orbit the camera with mouse
-}
mouseOrbiting : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Camera -> Camera
mouseOrbiting originalPosition newPosition (Camera camera) =
    let
        ( deltaX, deltaY ) =
            newPosition
                |> Vector2d.from originalPosition
                |> Vector2d.components

        radiansInPixels =
            orbitingPrecision (Camera camera)

        newAzimuth =
            camera.azimuth
                |> Quantity.minus (Quantity.at radiansInPixels deltaX)
                |> Angle.normalize

        newElevation =
            angleFromTimeline camera.elevation
                |> Quantity.plus (Quantity.at radiansInPixels deltaY)
                |> Quantity.clamp (Angle.degrees 6) (Angle.degrees 90)
    in
    Camera
        { camera
            | azimuth = newAzimuth
            , elevation = Animator.go Animator.immediately newElevation camera.elevation
        }


{-| Zoom in/out by mouse wheel delta
-}
mouseWheelZoom : Float -> Camera -> Camera
mouseWheelZoom deltaY (Camera camera) =
    let
        newZoom =
            clamp 0 1 (Animator.move camera.zoom Animator.at - deltaY * 0.002)
    in
    Camera { camera | zoom = Animator.go Animator.immediately newZoom camera.zoom }


{-| Read the angle value from the timeline
-}
angleFromTimeline : Timeline Angle -> Angle
angleFromTimeline angleTimeline =
    Angle.radians (Animator.move angleTimeline (Angle.inRadians >> Animator.at))



-- ANIMATION


{-| Animate the focal point of the camera to the new position
-}
focusOn : Point3d Meters WorldCoordinates -> Camera -> Camera
focusOn focalPoint (Camera camera) =
    Camera
        { camera
            | focalPoint = Animator.go Animator.quickly focalPoint camera.focalPoint
        }


{-| Zoom out the camera to look over the table from the top
-}
zoomOut : Camera -> Camera
zoomOut (Camera camera) =
    Camera
        { camera
            | zoom = Animator.go Animator.verySlowly 1 camera.zoom
            , elevation = Animator.go Animator.verySlowly (Angle.degrees 50) camera.elevation
        }


{-| Update the camera animation state, this needs to be called
from the animation frame subscription
-}
animate : Posix -> Camera -> Camera
animate time (Camera camera) =
    Camera
        { camera
            | elevation = Animator.updateTimeline time camera.elevation
            , zoom = Animator.updateTimeline time camera.zoom
            , focalPoint = Animator.updateTimeline time camera.focalPoint
        }
