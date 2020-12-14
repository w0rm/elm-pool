module Bodies exposing
    ( Data
    , Id(..)
    , areaBehindTheHeadString
    , areaBehindTheHeadStringEntity
    , balls
    , cueBall
    , floor
    , tableSurface
    , tableWalls
    )

import Angle
import Axis3d
import Block3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d
import Length exposing (Meters, meters, millimeters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape
import Point2d
import Point3d
import Pool exposing (Ball)
import Quantity exposing (Quantity(..))
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Vector3d


type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Walls


type alias Data =
    { entity : Entity BodyCoordinates
    , id : Id
    }


areaBehindTheHeadString : Rectangle3d Meters WorldCoordinates
areaBehindTheHeadString =
    Rectangle3d.on
        SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.meters
                -(sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -(sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
            (Point2d.meters
                (sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -((sizes.halfLength - sizes.wallThickness) / 2)
            )
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


areaBehindTheHeadStringEntity : Entity WorldCoordinates
areaBehindTheHeadStringEntity =
    case Rectangle3d.vertices areaBehindTheHeadString of
        [ v1, v2, v3, v4 ] ->
            Scene3d.quad
                (Material.nonmetal
                    { baseColor = Color.rgb255 80 80 0
                    , roughness = 1
                    }
                )
                v1
                v2
                v3
                v4

        _ ->
            Scene3d.nothing


radius : Float
radius =
    57.15 / 2


ballSphere : Sphere3d Meters BodyCoordinates
ballSphere =
    Sphere3d.atPoint
        (Point3d.millimeters 0 0 radius)
        (millimeters radius)


balls : Material.Texture Float -> Dict Int (Material.Texture Color) -> List (Body Data)
balls roughnessTexture ballTextures =
    let
        numbers =
            [ 1, 10, 4, 2, 8, 5, 9, 3, 14, 15, 11, 12, 6, 13, 7 ]

        lastRow =
            sqrt (2 * toFloat (List.length numbers)) - 1
    in
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
                { id =
                    Pool.numberedBall number
                        |> Maybe.map Numbered
                        |> Maybe.withDefault CueBall
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


cueBall : Body Data
cueBall =
    Body.sphere ballSphere
        { id = CueBall
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
                (Material.matte (Color.rgb255 46 52 54))
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
    , ballRadius : Float
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
    , ballRadius = 57.15 / 2000
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
