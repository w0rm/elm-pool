module Bodies exposing (Ball(..), Data, Id(..), balls, cueBall, floor, tableSurface, tableWalls)

import Angle
import Axis3d
import Block3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d
import Length exposing (Meters, meters, millimeters)
import Mass
import Palette.Tango as Tango
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape
import Point3d
import Quantity exposing (Quantity(..))
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SolidColor exposing (SolidColor)
import Sphere3d exposing (Sphere3d)
import Vector3d


type Ball
    = Numbered Int -- 1-15
    | Cue


type Id
    = Floor
    | Ball Ball
    | Table
    | Walls


type alias Data =
    { entity : Entity BodyCoordinates
    , id : Id
    }


radius : Float
radius =
    57.15 / 2


ballSphere : Sphere3d Meters BodyCoordinates
ballSphere =
    Sphere3d.atPoint
        (Point3d.millimeters 0 0 radius)
        (millimeters radius)


balls : Maybe (Material.Texture Float) -> Dict Int (Material.Texture Color) -> Maybe (List (Body Data))
balls maybeRoughnessTexture ballTextures =
    let
        numbers =
            [ 1, 10, 4, 2, 8, 5, 9, 3, 14, 15, 11, 12, 6, 13, 7 ]

        lastRow =
            sqrt (2 * toFloat (List.length numbers)) - 1
    in
    case ( maybeRoughnessTexture, Dict.size ballTextures ) of
        ( Just roughnessTexture, 15 ) ->
            List.indexedMap
                (\index number ->
                    let
                        row =
                            round (sqrt (2 * (toFloat index + 1))) - 1

                        rowStartIndex =
                            row * (row + 1) // 2

                        distance =
                            radius * sqrt 3

                        x =
                            (toFloat index - toFloat rowStartIndex - toFloat row / 2) * radius * 2

                        y =
                            (toFloat row - lastRow / 2) * distance

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
                    Body.sphere ballSphere
                        { id = Ball (Numbered number)
                        , entity =
                            Scene3d.sphereWithShadow
                                material
                                ballSphere
                                -- rotate to see the numbers
                                |> Scene3d.rotateAround
                                    (Axis3d.through (Sphere3d.centerPoint ballSphere) Direction3d.x)
                                    (Angle.degrees 90)
                        }
                        |> Body.withMaterial ballMaterial
                        |> Body.withDamping ballDamping
                        |> Body.withBehavior (Body.dynamic (Mass.grams 170))
                        |> Body.translateBy (Vector3d.millimeters x (y + 2100 / 4) 0)
                )
                numbers
                |> Just

        _ ->
            Nothing


cueBall : Body Data
cueBall =
    Body.sphere ballSphere
        { id = Ball Cue
        , entity =
            Scene3d.sphereWithShadow
                (Material.matte (Color.rgb255 255 255 255))
                ballSphere
        }
        |> Body.withMaterial ballMaterial
        |> Body.withDamping ballDamping
        |> Body.withBehavior (Body.dynamic (Mass.grams 170))
        |> Body.translateBy (Vector3d.meters 0 (-2.1 / 4) 0)


ballDamping : { linear : Float, angular : Float }
ballDamping =
    { linear = 0.4, angular = 0.4 }


ballMaterial : Material
ballMaterial =
    Physics.Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


floor : Body Data
floor =
    Body.plane
        { id = Floor
        , entity =
            Scene3d.quad
                (Material.matte <|
                    solidColorToColor Tango.aluminum6
                )
                (Point3d.meters -sizes.floorHalfSize -sizes.floorHalfSize 0)
                (Point3d.meters sizes.floorHalfSize -sizes.floorHalfSize 0)
                (Point3d.meters sizes.floorHalfSize sizes.floorHalfSize 0)
                (Point3d.meters -sizes.floorHalfSize sizes.floorHalfSize 0)
        }
        |> Body.moveTo (Point3d.meters 0 0 -sizes.height)


sizes :
    { halfWidth : Float
    , halfLength : Float
    , halfHole : Float
    , wallThickness : Float
    , wallHeight : Float
    , halfCornerHole : Float
    , halfCornerDiagonal : Float
    , height : Float
    , thickness : Float
    , floorHalfSize : Float
    }
sizes =
    let
        halfHole =
            0.05

        wallThickness =
            0.02

        halfCornerHole =
            sqrt (halfHole ^ 2) + wallThickness
    in
    { halfWidth = 1.1 / 2
    , halfLength = 2.1 / 2
    , halfHole = halfHole
    , wallThickness = wallThickness
    , wallHeight = 0.03
    , halfCornerHole = halfCornerHole
    , halfCornerDiagonal = sqrt (halfCornerHole ^ 2 / 2)
    , height = 0.45 -- distance from the floor until the top of the table
    , thickness = 0.03 -- the height of table top
    , floorHalfSize = 15
    }


tableSurface : Body Data
tableSurface =
    let
        cornerBlock =
            Block3d.from
                (Point3d.meters -sizes.halfCornerDiagonal -sizes.halfCornerDiagonal 0)
                (Point3d.meters sizes.halfCornerDiagonal sizes.halfCornerDiagonal -sizes.thickness)
                |> Block3d.rotateAround Axis3d.z (Angle.degrees 45)

        blocks =
            [ Block3d.from
                (Point3d.meters -sizes.halfWidth (-sizes.halfLength + sizes.halfCornerHole) 0)
                (Point3d.meters sizes.halfWidth (sizes.halfLength - sizes.halfCornerHole) -sizes.thickness)
            , Block3d.from
                (Point3d.meters (-sizes.halfWidth + sizes.halfCornerHole) (sizes.halfLength - sizes.halfCornerHole) 0)
                (Point3d.meters (sizes.halfWidth - sizes.halfCornerHole) sizes.halfLength -sizes.thickness)
            , Block3d.from
                (Point3d.meters (-sizes.halfWidth + sizes.halfCornerHole) (-sizes.halfLength + sizes.halfCornerHole) 0)
                (Point3d.meters (sizes.halfWidth - sizes.halfCornerHole) -sizes.halfLength -sizes.thickness)
            , Block3d.from
                (Point3d.meters (-sizes.halfWidth + sizes.halfCornerHole) (-sizes.halfLength + sizes.halfCornerHole) 0)
                (Point3d.meters (sizes.halfWidth - sizes.halfCornerHole) -sizes.halfLength -sizes.thickness)
            , Block3d.translateBy (Vector3d.meters (-sizes.halfWidth + sizes.halfCornerHole) (-sizes.halfLength + sizes.halfCornerHole) 0) cornerBlock
            , Block3d.translateBy (Vector3d.meters (sizes.halfWidth - sizes.halfCornerHole) (-sizes.halfLength + sizes.halfCornerHole) 0) cornerBlock
            , Block3d.translateBy (Vector3d.meters (-sizes.halfWidth + sizes.halfCornerHole) (sizes.halfLength - sizes.halfCornerHole) 0) cornerBlock
            , Block3d.translateBy (Vector3d.meters (sizes.halfWidth - sizes.halfCornerHole) (sizes.halfLength - sizes.halfCornerHole) 0) cornerBlock
            ]
    in
    Body.compound (List.map Physics.Shape.block blocks)
        { id = Table
        , entity =
            List.map
                (Scene3d.blockWithShadow
                    (Material.nonmetal
                        { baseColor = Color.rgb255 10 80 0
                        , roughness = 1
                        }
                    )
                )
                blocks
                |> Scene3d.group
        }
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.8
                , bounciness = 0
                }
            )


tableWalls : Body Data
tableWalls =
    let
        blocks =
            [ Block3d.from
                (Point3d.meters -sizes.halfWidth (-sizes.halfLength + sizes.halfCornerHole) sizes.wallHeight)
                (Point3d.meters (-sizes.halfWidth + sizes.wallThickness) -sizes.halfHole 0)
            , Block3d.from
                (Point3d.meters (sizes.halfWidth - sizes.wallThickness) -sizes.halfHole 0)
                (Point3d.meters sizes.halfWidth (-sizes.halfLength + sizes.halfCornerHole) sizes.wallHeight)
            , Block3d.from
                (Point3d.meters (sizes.halfWidth - sizes.halfCornerHole) (-sizes.halfLength + sizes.wallThickness) 0)
                (Point3d.meters (-sizes.halfWidth + sizes.halfCornerHole) -sizes.halfLength sizes.wallHeight)
            ]
                |> List.foldl
                    (\block result ->
                        Block3d.rotateAround Axis3d.z (Angle.degrees 180) block :: block :: result
                    )
                    []

        shapes =
            blocks
                |> List.map Physics.Shape.block

        entities =
            blocks
                |> List.map
                    (Scene3d.blockWithShadow
                        (Material.nonmetal
                            { baseColor = Color.rgb255 10 80 0
                            , roughness = 0.9
                            }
                        )
                    )
    in
    Body.compound shapes
        { id = Walls
        , entity = Scene3d.group entities
        }
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.3
                , bounciness = 0.6
                }
            )


solidColorToColor : SolidColor -> Color
solidColorToColor solidColor =
    let
        ( red, green, blue ) =
            SolidColor.toRGB solidColor
    in
    Color.rgb255 (round red) (round green) (round blue)
