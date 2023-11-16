module Bodies exposing
    ( Id(..)
    , areaBallInHand
    , areaBehindTheHeadString
    , areaBehindTheHeadStringEntity
    , ballRadius
    , bodyToEntity
    , clipDepth
    , cueBall
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
import Block3d exposing (Block3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d
import Dict exposing (Dict)
import Direction3d
import EightBall exposing (Ball)
import Length exposing (Length, Meters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape
import Physics.World as World exposing (World)
import Point2d
import Point3d
import Quantity
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Vector3d
import Viewpoint3d


type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Walls


areaBallInHand : Rectangle3d Meters WorldCoordinates
areaBallInHand =
    Rectangle3d.on
        SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.meters
                -(sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -(sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
            (Point2d.meters
                (sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                (sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


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
                    { baseColor = Color.rgb255 30 100 20
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


world : World Id
world =
    World.empty
        |> World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add tableSurface
        |> World.add tableWalls
        |> World.add floor
        |> (\w -> List.foldl World.add w balls)


ballRadius : Length
ballRadius =
    Length.millimeters (57.15 / 2)


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


ballSphere : Sphere3d Meters BodyCoordinates
ballSphere =
    Sphere3d.atOrigin ballRadius


balls : List (Body Id)
balls =
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
                    Length.inMillimeters ballRadius * sqrt 3

                x =
                    (toFloat index - toFloat rowStartIndex - toFloat row / 2) * Length.inMillimeters ballRadius * 2

                y =
                    (toFloat row - lastRow / 2) * distance
            in
            Body.sphere ballSphere
                (EightBall.numberedBall number
                    |> Maybe.map Numbered
                    |> Maybe.withDefault CueBall
                )
                |> Body.withMaterial ballMaterial
                |> Body.withDamping ballDamping
                |> Body.withBehavior (Body.dynamic (Mass.grams 170))
                |> Body.translateBy (Vector3d.millimeters x (y + 2100 / 4) (Length.inMillimeters ballRadius))
        )
        numbers


bodyToEntity : Material.Texture Float -> Dict Int (Material.Texture Color) -> Body Id -> Entity WorldCoordinates
bodyToEntity roughnessTexture ballTextures body =
    let
        id =
            Body.data body

        frame =
            Body.frame body

        entity =
            case id of
                Floor ->
                    Scene3d.quad
                        (Material.matte (Color.rgb255 46 52 54))
                        (Point3d.meters -sizes.floorHalfSize -sizes.floorHalfSize 0)
                        (Point3d.meters sizes.floorHalfSize -sizes.floorHalfSize 0)
                        (Point3d.meters sizes.floorHalfSize sizes.floorHalfSize 0)
                        (Point3d.meters -sizes.floorHalfSize sizes.floorHalfSize 0)

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
                    Scene3d.sphereWithShadow
                        material
                        ballSphere
                        -- rotate to see the numbers
                        |> Scene3d.rotateAround
                            (Axis3d.through (Sphere3d.centerPoint ballSphere) Direction3d.x)
                            (Angle.degrees 90)

                CueBall ->
                    Scene3d.sphereWithShadow
                        (Material.matte (Color.rgb255 255 255 255))
                        ballSphere

                Table ->
                    tableBlocks
                        |> List.map
                            (Scene3d.blockWithShadow
                                (Material.nonmetal
                                    { baseColor = Color.rgb255 10 80 0
                                    , roughness = 1
                                    }
                                )
                            )
                        |> Scene3d.group

                Walls ->
                    wallsBlocks
                        |> List.map
                            (Scene3d.blockWithShadow
                                (Material.nonmetal
                                    { baseColor = Color.rgb255 10 80 0
                                    , roughness = 0.9
                                    }
                                )
                            )
                        |> Scene3d.group
    in
    Scene3d.placeIn frame entity


cueBall : Body Id
cueBall =
    Body.sphere ballSphere CueBall
        |> Body.withMaterial ballMaterial
        |> Body.withDamping ballDamping
        |> Body.withBehavior (Body.dynamic (Mass.grams 170))


cueBallEntity : Bool -> Scene3d.Entity BodyCoordinates
cueBallEntity isActive =
    Scene3d.sphereWithShadow
        (if isActive then
            Material.matte Color.white

         else
            Material.matte inactiveColor
        )
        ballSphere


ballDamping : { linear : Float, angular : Float }
ballDamping =
    { linear = 0.4, angular = 0.4 }


ballMaterial : Material
ballMaterial =
    Physics.Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


floor : Body Id
floor =
    Body.plane Floor
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


tableBlocks : List (Block3d Meters BodyCoordinates)
tableBlocks =
    let
        cornerBlock =
            Block3d.from
                (Point3d.meters -sizes.halfCornerDiagonal -sizes.halfCornerDiagonal 0)
                (Point3d.meters sizes.halfCornerDiagonal sizes.halfCornerDiagonal -sizes.thickness)
                |> Block3d.rotateAround Axis3d.z (Angle.degrees 45)
    in
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


tableSurface : Body Id
tableSurface =
    Body.compound (List.map Physics.Shape.block tableBlocks) Table
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.8
                , bounciness = 0
                }
            )


wallsBlocks : List (Block3d Meters BodyCoordinates)
wallsBlocks =
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


tableWalls : Body Id
tableWalls =
    Body.compound (List.map Physics.Shape.block wallsBlocks) Walls
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.1
                , bounciness = 0.8
                }
            )
