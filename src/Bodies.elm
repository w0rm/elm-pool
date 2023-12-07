module Bodies exposing
    ( Id(..)
    , areaBehindTheHeadStringEntity
    , bodyToEntity
    , clipDepth
    , cueBallEntity
    , cueEntity
    , cueLength
    , cueOffset
    , cueRadius
    , world
    )

import Acceleration
import Angle
import Axis3d exposing (Axis3d)
import Ball
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d
import Dict exposing (Dict)
import Direction3d
import EightBall exposing (Ball)
import Length exposing (Length, Meters)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.World as World exposing (World)
import Point3d
import Quantity
import Rectangle3d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Table exposing (Table)
import Vector3d
import Viewpoint3d


type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Cushion
    | Pocket


areaBehindTheHeadStringEntity : Entity WorldCoordinates
areaBehindTheHeadStringEntity =
    case Rectangle3d.vertices Table.areaBehindTheHeadString of
        [ v1, v2, v3, v4 ] ->
            Scene3d.quad
                (Material.nonmetal
                    { baseColor = Color.rgb255 131 146 34
                    , roughness = 1
                    }
                )
                v1
                v2
                v3
                v4

        _ ->
            Scene3d.nothing


inactiveColor : Color
inactiveColor =
    Color.rgb255 130 130 130


cueEntity : Camera3d Meters WorldCoordinates -> Axis3d Meters WorldCoordinates -> Bool -> Scene3d.Entity WorldCoordinates
cueEntity camera3d axis isActive =
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
                                |> Quantity.minus cueOffset
                    in
                    if Quantity.lessThanOrEqualTo cueLength distanceFromCamera then
                        distanceFromCamera

                    else
                        cueLength

                Nothing ->
                    cueLength

        maybeCylinder =
            Cylinder3d.from
                (Point3d.along axis cueOffset)
                (Point3d.along axis (Quantity.plus trimmedCueLength cueOffset))
                cueRadius
    in
    case maybeCylinder of
        Just cylinder ->
            Scene3d.cylinderWithShadow
                (Material.nonmetal
                    { baseColor =
                        if isActive then
                            Color.white

                        else
                            inactiveColor
                    , roughness = 0.6
                    }
                )
                cylinder

        Nothing ->
            Scene3d.nothing


world : Table -> World Id
world table =
    World.empty
        |> World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add
            (Body.compound table.table Table
                |> Body.withMaterial
                    (Physics.Material.custom
                        { friction = 0.8
                        , bounciness = 0
                        }
                    )
            )
        |> World.add
            (Body.compound table.cushions Cushion
                |> Body.withMaterial
                    (Physics.Material.custom
                        { friction = 0.1
                        , bounciness = 0.8
                        }
                    )
            )
        |> World.add (Body.compound table.pockets Pocket)
        |> World.add
            (Body.plane Floor
                -- distance from the floor until the top of the table
                |> Body.moveTo (Point3d.meters 0 0 -0.45)
            )
        |> (\w -> List.foldl World.add w rackedBalls)


cueLength : Length
cueLength =
    Length.centimeters 150


cueRadius : Length
cueRadius =
    Length.millimeters 6


{-| Cue offset from the cue ball
-}
cueOffset : Length
cueOffset =
    Length.centimeters 2


clipDepth : Length
clipDepth =
    Length.meters 0.1


rackedBalls : List (Body Id)
rackedBalls =
    let
        -- TODO: randomly shuffle the balls?
        numbers =
            [ 1, 10, 4, 2, 8, 5, 9, 3, 14, 15, 11, 12, 6, 13, 7 ]
    in
    List.indexedMap
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
                        (Quantity.multiplyBy (toFloat row * sqrt 3) Ball.radius)
                        (Quantity.multiplyBy (toFloat (index * 2 - row * (row + 2))) Ball.radius)
                        Ball.radius

                position =
                    Table.footSpot
                        |> Point3d.on SketchPlane3d.xy
                        |> Point3d.translateBy offset
            in
            Ball.body
                (EightBall.numberedBall number
                    |> Maybe.map Numbered
                    |> Maybe.withDefault CueBall
                )
                |> Body.moveTo position
        )
        numbers


bodyToEntity : Material.Texture Float -> Dict Int (Material.Texture Color) -> Table -> Body Id -> Entity WorldCoordinates
bodyToEntity roughnessTexture ballTextures table body =
    Scene3d.placeIn (Body.frame body) <|
        case Body.data body of
            Floor ->
                Scene3d.quad
                    (Material.matte (Color.rgb255 46 52 54))
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters 15 -15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters -15 15 0)

            Numbered ball ->
                let
                    number =
                        EightBall.ballNumber ball

                    colorTexture =
                        Dict.get number ballTextures
                            |> Maybe.withDefault (Material.constant (Color.rgb255 0 0 0))

                    material =
                        Material.texturedPbr
                            { baseColor = colorTexture
                            , roughness = roughnessTexture
                            , metallic = Material.constant 0
                            }
                in
                Scene3d.sphereWithShadow material Ball.sphere
                    -- rotate to see the numbers
                    |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)

            CueBall ->
                cueBallEntity True roughnessTexture

            Table ->
                Scene3d.meshWithShadow table.material table.mesh table.shadow

            _ ->
                Scene3d.nothing


cueBallEntity : Bool -> Material.Texture Float -> Scene3d.Entity BodyCoordinates
cueBallEntity isActive roughnessTexture =
    Scene3d.sphereWithShadow
        (if isActive then
            Material.texturedPbr
                { baseColor = Material.constant Color.white
                , roughness = roughnessTexture
                , metallic = Material.constant 0
                }

         else
            Material.matte inactiveColor
        )
        Ball.sphere
