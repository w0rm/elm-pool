module Cue exposing (entity, length, offset, radius)

import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Length exposing (Length, Meters)
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d
import Quantity
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Viewpoint3d


length : Length
length =
    Length.centimeters 150


radius : Length
radius =
    Length.millimeters 6


{-| Cue offset from the cue ball
-}
offset : Length
offset =
    Length.centimeters 2


entity : Camera3d Meters WorldCoordinates -> Length -> Axis3d Meters WorldCoordinates -> Color -> Entity WorldCoordinates
entity camera3d clipDepth axis color =
    case cylinder camera3d clipDepth axis of
        Just trimmedCylinder ->
            Scene3d.cylinderWithShadow
                (Material.nonmetal
                    { baseColor = color
                    , roughness = 0.6
                    }
                )
                trimmedCylinder

        Nothing ->
            Scene3d.nothing


cylinder : Camera3d Meters WorldCoordinates -> Length -> Axis3d Meters WorldCoordinates -> Maybe (Cylinder3d Meters WorldCoordinates)
cylinder camera3d clipDepth axis =
    let
        viewPlane =
            camera3d
                |> Camera3d.viewpoint
                |> Viewpoint3d.viewPlane
                |> SketchPlane3d.toPlane

        trimmedCueLength =
            -- shorten the cue cylinder if intersects with the view plane
            case Axis3d.intersectionWithPlane viewPlane axis of
                Just point ->
                    let
                        distanceFromCamera =
                            Point3d.distanceFrom (Axis3d.originPoint axis) point
                                |> Quantity.minus clipDepth
                                -- minus the offset from the cue ball
                                |> Quantity.minus offset
                    in
                    if Quantity.lessThanOrEqualTo length distanceFromCamera then
                        distanceFromCamera

                    else
                        length

                Nothing ->
                    length
    in
    Cylinder3d.from
        (Point3d.along axis offset)
        (Point3d.along axis (Quantity.plus trimmedCueLength offset))
        radius