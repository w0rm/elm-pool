module Bodies exposing
    ( Id(..)
    , areaBehindTheHeadStringEntity
    , bodyToEntity
    , cueBallEntity
    , cueEntity
    , world
    )

import Acceleration
import Angle
import Axis3d exposing (Axis3d)
import Ball
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cue
import Dict exposing (Dict)
import Direction3d
import EightBall exposing (Ball)
import Length exposing (Length, Meters)
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.World as World exposing (World)
import Point3d
import Rectangle3d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Table exposing (Table)



-- PHYSICAL WORLD


type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Cushion
    | Pocket


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
        |> (\w ->
                List.foldl World.add w <|
                    Ball.rack
                        Table.footSpot
                        (EightBall.numberedBall >> Maybe.map Numbered)
           )



-- VISUAL ENTITIES


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
                    colorTexture =
                        Dict.get (EightBall.ballNumber ball) ballTextures
                            |> Maybe.withDefault (Material.constant Color.black)

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
        (Material.texturedPbr
            { baseColor =
                if isActive then
                    Material.constant Color.white

                else
                    Material.constant inactiveColor
            , roughness = roughnessTexture
            , metallic = Material.constant 0
            }
        )
        Ball.sphere


{-| Highlight the area where the ball should be placed
-}
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


cueEntity : Camera3d Meters WorldCoordinates -> Length -> Axis3d Meters WorldCoordinates -> Bool -> Scene3d.Entity WorldCoordinates
cueEntity camera3d clipDepth axis isActive =
    case Cue.cylinder camera3d clipDepth axis of
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


inactiveColor : Color
inactiveColor =
    Color.rgb255 130 130 130
