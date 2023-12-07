module Ball exposing (body, rack, radius, sphere)

import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material as Material exposing (Material)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Vector3d


radius : Length
radius =
    Length.millimeters (57.15 / 2)


weight : Mass
weight =
    Mass.grams 170


{-| The perfect sphere keeps rolling on the perfect plane,
we need to damp the angular velocity to make it stop.
-}
damping : { linear : Float, angular : Float }
damping =
    { linear = 0.4, angular = 0.4 }


ballMaterial : Material
ballMaterial =
    Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


sphere : Sphere3d Meters BodyCoordinates
sphere =
    Sphere3d.atOrigin radius


body : id -> Body id
body id =
    Body.sphere sphere
        id
        |> Body.withMaterial ballMaterial
        |> Body.withDamping damping
        |> Body.withBehavior (Body.dynamic weight)


rack : Point2d Meters WorldCoordinates -> (Int -> Maybe id) -> List (Body id)
rack footSpot fn =
    let
        -- TODO: randomly shuffle the balls?
        numbers =
            [ 1, 10, 4, 2, 8, 5, 9, 3, 14, 15, 11, 12, 6, 13, 7 ]
    in
    numbers
        |> List.indexedMap
            (\index number ->
                let
                    -- index:
                    --         14
                    --       9
                    --     5   13
                    --   2   8
                    -- 0   4   12
                    --   1   7
                    --     3   11
                    --       6
                    --         10
                    -- row:
                    -- 0 1 2 3 4
                    row =
                        round (sqrt (2 * (toFloat index + 1))) - 1

                    offset =
                        Vector3d.xyz
                            (Quantity.multiplyBy (toFloat row * sqrt 3) radius)
                            (Quantity.multiplyBy (toFloat (index * 2 - row * (row + 2))) radius)
                            radius

                    position =
                        footSpot
                            |> Point3d.on SketchPlane3d.xy
                            |> Point3d.translateBy offset
                in
                ( fn number, position )
            )
        |> List.filterMap
            (\( maybeId, pos ) ->
                Maybe.map (body >> Body.moveTo pos) maybeId
            )
