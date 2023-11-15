module Game exposing
    ( Model
    , Msg(..)
    , State(..)
    , currentPlayer
    , currentTarget
    , initial
    , subscriptions
    , update
    , view
    )

import Angle exposing (Angle)
import Animator exposing (Timeline)
import Axis3d exposing (Axis3d)
import Bodies exposing (Id(..))
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d
import Dict exposing (Dict)
import Direction3d
import Duration
import EightBall exposing (AwaitingPlaceBallBehindHeadstring, AwaitingPlaceBallInHand, AwaitingPlayerShot, AwaitingStart, Player, Pool, ShotEvent, WhatHappened(..))
import Force
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Meters)
import List
import Physics.Body as Body
import Physics.Contact as Contact
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (Pixels, pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material
import Set exposing (Set)
import SketchPlane3d
import Speed
import Time exposing (Posix)
import Vector2d
import Vector3d
import Viewpoint3d


type ScreenCoordinates
    = ScreenCoordinates Never


type alias Model =
    { world : World Id
    , ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Material.Texture Float
    , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
    , orbiting : Maybe (Point2d Pixels ScreenCoordinates)
    , state : State
    , time : Posix

    -- Animated camera properties:
    , zoom : Timeline Float
    , azimuth : Timeline Angle
    , elevation : Timeline Angle
    , focalPoint : Timeline (Point3d Meters WorldCoordinates)
    }


type State
    = PlacingBehindHeadString PlacingBallMouse (Pool AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingMouse PlayingState (Pool AwaitingPlayerShot)
    | Simulating (List ( Time.Posix, ShotEvent )) (Pool AwaitingPlayerShot)
    | PlacingBallInHand PlacingBallMouse (Pool AwaitingPlaceBallInHand)
    | GameOver Player (Pool AwaitingStart)


type alias PlayingState =
    { cueBallPosition : Point3d Meters WorldCoordinates
    , cueElevation : Angle
    , hitElevation : Angle
    , hitRelativeAzimuth : Angle -- offset from camera azimuth
    , shootButton : Maybe Posix -- the time when the button was pressed
    }


type
    PlayingMouse
    -- TODO: use HoveringCueBall to render the clickable area
    = HoveringCueBall Angle Angle -- elevation and relative azimuth
    | SettingCueElevation (Point2d Pixels ScreenCoordinates)
    | OutsideOfCueBall


initialPlayingState : Point3d Meters WorldCoordinates -> PlayingState
initialPlayingState cueBallPosition =
    { cueBallPosition = cueBallPosition
    , cueElevation = Angle.degrees 5
    , hitRelativeAzimuth = Angle.degrees 0
    , hitElevation = Angle.degrees 0
    , shootButton = Nothing
    }


type PlacingBallMouse
    = CanSpawnAt (Point3d Meters WorldCoordinates)
    | CannotSpawn (Point3d Meters WorldCoordinates)
    | HoveringOuside


type Msg
    = Tick Time.Posix
    | Resize Int Int
    | MouseWheel Float
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | ShootButtonDown
    | ShootButtonUp


initial : Dict Int (Material.Texture Color) -> Material.Texture Float -> ( Float, Float ) -> Model
initial ballTextures roughnessTexture ( width, height ) =
    let
        time =
            -- TODO: consider getting the initial time
            Time.millisToPosix 0
    in
    { world = Bodies.world
    , ballTextures = ballTextures
    , roughnessTexture = roughnessTexture
    , time = time
    , dimensions = ( Pixels.float width, Pixels.float height )
    , zoom = Animator.init 0.9
    , azimuth = Animator.init (Angle.degrees -25)
    , elevation = Animator.init (Angle.degrees 30)
    , focalPoint = Animator.init Point3d.origin
    , orbiting = Nothing
    , state = PlacingBehindHeadString HoveringOuside (EightBall.rack time EightBall.start)
    }


camera : Model -> Camera3d Meters WorldCoordinates
camera { azimuth, elevation, zoom, focalPoint } =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint =
                    Point3d.meters
                        (Animator.linear focalPoint (Point3d.xCoordinate >> Length.inMeters >> Animator.at))
                        (Animator.linear focalPoint (Point3d.yCoordinate >> Length.inMeters >> Animator.at))
                        (Animator.linear focalPoint (Point3d.zCoordinate >> Length.inMeters >> Animator.at))
                , groundPlane = SketchPlane3d.xy
                , azimuth = angleFromTimeline azimuth
                , elevation = angleFromTimeline elevation
                , distance = Quantity.interpolateFrom (Length.meters 0.5) (Length.meters 5) (Animator.linear zoom Animator.at)
                }
        , verticalFieldOfView = Angle.degrees 24
        }


ray : Model -> Point2d Pixels ScreenCoordinates -> Axis3d Meters WorldCoordinates
ray model =
    Camera3d.ray
        (camera model)
        (Rectangle2d.with
            { x1 = pixels 0
            , y1 = Tuple.second model.dimensions
            , x2 = Tuple.first model.dimensions
            , y2 = pixels 0
            }
        )


view : Model -> Html Msg
view ({ world, ballTextures, roughnessTexture, dimensions } as model) =
    let
        dimensionsInt =
            Tuple.mapBoth Quantity.round Quantity.round dimensions

        sunlight =
            Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , intensity = Illuminance.lux 10000
                , chromaticity = Scene3d.Light.daylight
                }

        environmentalLighting =
            Scene3d.Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Scene3d.Light.daylight
                , intensityAbove = Illuminance.lux 3000
                , intensityBelow = Illuminance.lux 0
                }

        camera3d =
            camera model

        bodies =
            case model.state of
                PlacingBehindHeadString _ _ ->
                    World.bodies world
                        |> List.filter
                            (\b ->
                                Body.data b /= CueBall
                            )

                _ ->
                    World.bodies world

        entities =
            List.map
                (Bodies.bodyToEntity roughnessTexture ballTextures)
                bodies

        entitiesWithUI =
            case model.state of
                PlacingBehindHeadString mouse _ ->
                    placingBallEntities mouse Bodies.areaBehindTheHeadStringEntity :: entities

                PlacingBallInHand mouse _ ->
                    placingBallEntities mouse Scene3d.nothing :: entities

                Playing _ playingState _ ->
                    let
                        axis =
                            cueAxis playingState model.azimuth
                    in
                    cueEntity world camera3d axis :: entities

                _ ->
                    entities

        cursor =
            case model.state of
                PlacingBallInHand _ _ ->
                    "none"

                PlacingBehindHeadString _ _ ->
                    "none"

                Playing (HoveringCueBall _ _) _ _ ->
                    "pointer"

                Playing (SettingCueElevation _) _ _ ->
                    "ns-resize"

                Simulating _ _ ->
                    "wait"

                _ ->
                    "default"
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "cursor" cursor
        , Html.Events.preventDefaultOn "wheel"
            (Json.Decode.map
                (\deltaY -> ( MouseWheel deltaY, True ))
                (Json.Decode.field "deltaY" Json.Decode.float)
            )
        ]
        [ Scene3d.custom
            { dimensions = dimensionsInt
            , antialiasing = Scene3d.noAntialiasing
            , camera = camera3d
            , entities = entitiesWithUI
            , lights = Scene3d.twoLights environmentalLighting sunlight
            , exposure = Scene3d.exposureValue 13
            , whiteBalance = Scene3d.Light.daylight
            , clipDepth = Length.meters 0.1
            , background = Scene3d.backgroundColor Color.black
            , toneMapping = Scene3d.noToneMapping
            }
        , viewShootingStrength model
        ]


cueAxis : PlayingState -> Timeline Angle -> Axis3d Meters WorldCoordinates
cueAxis { hitRelativeAzimuth, cueElevation, cueBallPosition, hitElevation } cameraAzimuthTimeline =
    let
        cameraAzimuth =
            angleFromTimeline cameraAzimuthTimeline

        hitAzimuth =
            cameraAzimuth
                |> Quantity.plus hitRelativeAzimuth

        pointDirection =
            Direction3d.xyZ hitAzimuth hitElevation

        point =
            cueBallPosition
                |> Point3d.translateIn pointDirection (Length.millimeters (57.15 / 2))

        axisDirection =
            Direction3d.xyZ cameraAzimuth cueElevation
    in
    Axis3d.through point axisDirection


{-| Check if the cue doesn't overlap with any other objects
-}
canShoot : Axis3d Meters WorldCoordinates -> World Id -> Bool
canShoot axis world =
    let
        direction =
            Axis3d.direction axis

        originPoint =
            Axis3d.originPoint axis

        pointOnCue =
            Point3d.translateIn
                (Direction3d.perpendicularTo direction)
                cueRadius
                originPoint

        cueRadius =
            Length.millimeters 6

        cueMaxDistance =
            Length.centimeters (2 + 150)

        worldWithoutCueBall =
            World.keepIf (\b -> Body.data b /= CueBall) world
    in
    -- cast 8 rays on the surface of the cylinder
    List.all
        (\n ->
            let
                angle =
                    Angle.degrees (360 * toFloat n / 8)

                origin =
                    Point3d.rotateAround axis angle pointOnCue

                cueRay =
                    Axis3d.through origin direction
            in
            case World.raycast cueRay worldWithoutCueBall of
                Just { point, body } ->
                    let
                        frame =
                            Body.frame body

                        distance =
                            Point3d.distanceFrom originPoint (Point3d.placeIn frame point)
                    in
                    Quantity.greaterThan cueMaxDistance distance

                Nothing ->
                    True
        )
        (List.range 0 7)


placingBallEntities : PlacingBallMouse -> Scene3d.Entity WorldCoordinates -> Scene3d.Entity WorldCoordinates
placingBallEntities placingBall areaEntity =
    case placingBall of
        CanSpawnAt position ->
            Scene3d.group
                [ areaEntity
                , Scene3d.sphereWithShadow
                    (Material.matte (Color.rgb255 255 255 255))
                    Bodies.ballSphere
                    |> Scene3d.placeIn (Frame3d.atPoint position)
                ]

        CannotSpawn position ->
            Scene3d.group
                [ areaEntity
                , Scene3d.sphereWithShadow
                    (Material.matte inactiveColor)
                    Bodies.ballSphere
                    |> Scene3d.placeIn (Frame3d.atPoint position)
                ]

        HoveringOuside ->
            Scene3d.nothing


inactiveColor : Color
inactiveColor =
    Color.rgb255 130 130 130


cueEntity : World Id -> Camera3d Meters WorldCoordinates -> Axis3d Meters WorldCoordinates -> Scene3d.Entity WorldCoordinates
cueEntity world camera3d axis =
    let
        viewpoint =
            Camera3d.viewpoint camera3d

        viewPlane =
            SketchPlane3d.toPlane (Viewpoint3d.viewPlane viewpoint)

        cueMaxDistance =
            Length.centimeters (2 + 150)

        cueRadius =
            Length.millimeters 6

        cueDistance =
            -- shorten the cue cylinder if intersects with the view plane
            case Axis3d.intersectionWithPlane viewPlane axis of
                Just point ->
                    let
                        distanceFromCamera =
                            Point3d.distanceFrom (Axis3d.originPoint axis) point
                                --minus the clipDepth
                                |> Quantity.minus (Length.meters 0.1)
                    in
                    if Quantity.lessThanOrEqualTo cueMaxDistance distanceFromCamera then
                        distanceFromCamera

                    else
                        cueMaxDistance

                Nothing ->
                    cueMaxDistance

        maybeCylinder =
            Cylinder3d.from
                (Point3d.along axis (Length.centimeters 2))
                (Point3d.along axis cueDistance)
                cueRadius
    in
    case maybeCylinder of
        Just cylinder ->
            Scene3d.cylinderWithShadow
                (Material.nonmetal
                    { baseColor =
                        if canShoot axis world then
                            Color.white

                        else
                            inactiveColor
                    , roughness = 0.6
                    }
                )
                cylinder

        Nothing ->
            Scene3d.nothing


viewShootingStrength : Model -> Html Msg
viewShootingStrength { state, time, dimensions } =
    case state of
        Playing _ { shootButton } _ ->
            case shootButton of
                Nothing ->
                    Html.text ""

                Just startTime ->
                    let
                        progressHeight =
                            shootingStrength startTime time * (barHeight - 4)

                        height =
                            Tuple.second dimensions |> Pixels.inPixels

                        barHeight =
                            height * 0.6

                        barBottom =
                            (height - barHeight) / 2
                    in
                    Html.div []
                        [ Html.div
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "right" "50px"
                            , Html.Attributes.style "bottom" (String.fromFloat barBottom ++ "px")
                            , Html.Attributes.style "width" "40px"
                            , Html.Attributes.style "height" (String.fromFloat barHeight ++ "px")
                            , Html.Attributes.style "border" "2px solid #fff"
                            , Html.Attributes.style "border-radius" "10px"
                            ]
                            []
                        , Html.div
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "right" "54px"
                            , Html.Attributes.style "bottom" (String.fromFloat (barBottom + 4) ++ "px")
                            , Html.Attributes.style "width" "36px"
                            , Html.Attributes.style "background" "#fff"
                            , Html.Attributes.style "border-radius" "6px"
                            , Html.Attributes.style "height" (String.fromFloat progressHeight ++ "px")
                            ]
                            []
                        ]

        _ ->
            Html.text ""


currentPlayer : Model -> Player
currentPlayer model =
    case model.state of
        PlacingBehindHeadString _ pool ->
            EightBall.currentPlayer pool

        Playing _ _ pool ->
            EightBall.currentPlayer pool

        Simulating _ pool ->
            EightBall.currentPlayer pool

        PlacingBallInHand _ pool ->
            EightBall.currentPlayer pool

        GameOver winner _ ->
            winner


currentTarget : State -> String
currentTarget state =
    let
        currentTarget_ : EightBall.CurrentTarget
        currentTarget_ =
            case state of
                PlacingBehindHeadString _ pool ->
                    EightBall.currentTarget pool

                Playing _ _ pool ->
                    EightBall.currentTarget pool

                Simulating _ pool ->
                    EightBall.currentTarget pool

                PlacingBallInHand _ pool ->
                    EightBall.currentTarget pool

                GameOver _ pool ->
                    EightBall.currentTarget pool
    in
    case currentTarget_ of
        EightBall.OpenTable ->
            "Open Table"

        EightBall.Solids ->
            "Solids"

        EightBall.Stripes ->
            "Stripes"

        EightBall.EightBall ->
            "8-Ball"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onKeyDown (decodeKey ShootButtonDown)
        , Browser.Events.onKeyUp (decodeKey ShootButtonUp)
        , Browser.Events.onAnimationFrame Tick
        , Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


ballsStoppedMoving : World Id -> Bool
ballsStoppedMoving world =
    List.all
        (\body ->
            case Body.data body of
                CueBall ->
                    Body.velocity body
                        |> Vector3d.length
                        |> Quantity.lessThan (Speed.metersPerSecond 0.0005)

                Numbered _ ->
                    Body.velocity body
                        |> Vector3d.length
                        |> Quantity.lessThan (Speed.metersPerSecond 0.0005)

                _ ->
                    True
        )
        (World.bodies world)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            let
                -- advance the timelines
                newModel =
                    { model
                        | time = time
                        , azimuth = Animator.updateTimeline time model.azimuth
                        , elevation = Animator.updateTimeline time model.elevation
                        , zoom = Animator.updateTimeline time model.zoom
                        , focalPoint = Animator.updateTimeline time model.focalPoint
                    }
            in
            case newModel.state of
                Simulating events pool ->
                    if ballsStoppedMoving newModel.world then
                        case EightBall.playerShot (List.reverse events) pool of
                            IllegalBreak newPool ->
                                { newModel
                                    | world = Bodies.world -- Reset the table.
                                    , state = PlacingBehindHeadString HoveringOuside (EightBall.rack time newPool)
                                    , focalPoint = Animator.go Animator.quickly Point3d.origin newModel.focalPoint
                                }

                            PlayersFault newPool ->
                                { newModel
                                    | state = PlacingBallInHand HoveringOuside newPool
                                    , world = World.keepIf (\b -> Body.data b /= CueBall) newModel.world
                                    , focalPoint = Animator.go Animator.quickly Point3d.origin newModel.focalPoint
                                }

                            NextShot newPool ->
                                let
                                    cuePosition =
                                        World.bodies newModel.world
                                            |> List.filter (\b -> Body.data b == CueBall)
                                            |> List.head
                                            |> Maybe.map (\b -> Frame3d.originPoint (Body.frame b))
                                            |> Maybe.withDefault Point3d.origin
                                in
                                { newModel
                                    | state = Playing OutsideOfCueBall (initialPlayingState cuePosition) newPool
                                    , focalPoint = Animator.go Animator.quickly cuePosition newModel.focalPoint
                                }

                            EightBall.GameOver newPool { winner } ->
                                { newModel
                                    | state = GameOver winner newPool
                                    , focalPoint = Animator.go Animator.quickly Point3d.origin newModel.focalPoint
                                }

                    else
                        let
                            ( newWorld, newEvents ) =
                                simulateWithEvents 2 time newModel.world events
                        in
                        { newModel
                            | state = Simulating newEvents pool
                            , world = newWorld
                        }

                _ ->
                    newModel

        Resize width height ->
            { model | dimensions = ( Pixels.float (toFloat width), Pixels.float (toFloat height) ) }

        MouseWheel deltaY ->
            let
                newZoom =
                    clamp 0 1 (Animator.linear model.zoom Animator.at - deltaY * 0.002)
            in
            { model | zoom = Animator.go Animator.immediately newZoom model.zoom }

        MouseDown mousePosition ->
            case model.state of
                PlacingBallInHand (CanSpawnAt position) pool ->
                    { model
                        | state =
                            Playing OutsideOfCueBall
                                (initialPlayingState position)
                                (EightBall.placeBallInHand model.time pool)
                        , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                        , focalPoint = Animator.go Animator.quickly position model.focalPoint
                    }

                PlacingBehindHeadString (CanSpawnAt position) pool ->
                    { model
                        | state =
                            Playing OutsideOfCueBall
                                (initialPlayingState position)
                                (EightBall.placeBallBehindHeadstring model.time pool)
                        , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                        , focalPoint = Animator.go Animator.quickly position model.focalPoint
                    }

                -- TODO: decide if we want to prevent orbiting in this cases
                PlacingBehindHeadString (CannotSpawn _) _ ->
                    model

                PlacingBallInHand (CannotSpawn _) _ ->
                    model

                Playing (HoveringCueBall hitRelativeAzimuth hitElevation) playingState pool ->
                    let
                        newPlayingState =
                            { playingState
                                | hitRelativeAzimuth = hitRelativeAzimuth
                                , hitElevation = hitElevation
                            }
                    in
                    { model | state = Playing (SettingCueElevation mousePosition) newPlayingState pool }

                _ ->
                    { model | orbiting = Just mousePosition }

        MouseMove mousePosition ->
            case model.orbiting of
                Just originalPosition ->
                    -- update the camera orientation
                    -- note that changing the azimuth impacts the cue axis
                    mouseOrbiting originalPosition mousePosition model

                Nothing ->
                    case model.state of
                        PlacingBallInHand _ pool ->
                            let
                                newMouse =
                                    canSpawnHere (ray model mousePosition) Bodies.areaBallInHand model.world
                            in
                            { model | state = PlacingBallInHand newMouse pool }

                        PlacingBehindHeadString _ pool ->
                            let
                                newMouse =
                                    canSpawnHere (ray model mousePosition) Bodies.areaBehindTheHeadString model.world
                            in
                            { model | state = PlacingBehindHeadString newMouse pool }

                        Playing (SettingCueElevation originalPosition) playingState pool ->
                            let
                                newPlayingState =
                                    setCueElevation originalPosition mousePosition model.zoom playingState
                            in
                            { model | state = Playing (SettingCueElevation mousePosition) newPlayingState pool }

                        Playing _ playingState pool ->
                            let
                                newMouse =
                                    hoverCueBall (ray model mousePosition) model.world model.azimuth
                            in
                            { model | state = Playing newMouse playingState pool }

                        _ ->
                            model

        MouseUp ->
            case model.state of
                Playing _ playingState pool ->
                    { model
                        | state = Playing OutsideOfCueBall playingState pool
                        , orbiting = Nothing
                    }

                _ ->
                    { model | orbiting = Nothing }

        ShootButtonDown ->
            case model.state of
                Playing mouse playingState pool ->
                    let
                        axis =
                            cueAxis playingState model.azimuth
                    in
                    -- ShootButtonDown can be sent many times
                    -- we need to check if it isn't already pressed
                    if canShoot axis model.world && playingState.shootButton == Nothing then
                        { model
                            | state =
                                -- save the time the buttom was pressed
                                Playing mouse { playingState | shootButton = Just model.time } pool
                        }

                    else
                        model

                _ ->
                    model

        ShootButtonUp ->
            case model.state of
                Playing mouse playingState pool ->
                    let
                        axis =
                            cueAxis playingState model.azimuth
                    in
                    case ( canShoot axis model.world, playingState.shootButton ) of
                        ( True, Just startTime ) ->
                            { model
                                | state = Simulating [] pool
                                , zoom = Animator.go Animator.verySlowly 1 model.zoom
                                , elevation = Animator.go Animator.verySlowly (Angle.degrees 50) model.elevation
                                , world = shoot axis startTime model.time (EightBall.isBreak pool) model.world
                            }

                        _ ->
                            { model | state = Playing mouse { playingState | shootButton = Nothing } pool }

                _ ->
                    model


hoverCueBall : Axis3d Meters WorldCoordinates -> World Id -> Timeline Angle -> PlayingMouse
hoverCueBall mouseRay world azimuthTimeline =
    case World.raycast mouseRay world of
        Just { body, normal } ->
            let
                frame =
                    Body.frame body

                hitNormal =
                    Direction3d.placeIn frame normal

                hitAzimuth =
                    Direction3d.azimuthIn SketchPlane3d.xy hitNormal

                hitElevation =
                    Direction3d.elevationFrom SketchPlane3d.xy hitNormal

                azimuth =
                    angleFromTimeline azimuthTimeline

                hitRelativeAzimuth =
                    Quantity.minus azimuth hitAzimuth
                        |> Angle.normalize

                hoveringFrontHemisphere =
                    -- Prevent from hoveing the back hemisphere when looking from the top
                    Quantity.lessThan (Angle.degrees 90) (Quantity.abs hitRelativeAzimuth)
            in
            if Body.data body == CueBall && hoveringFrontHemisphere then
                HoveringCueBall hitRelativeAzimuth hitElevation

            else
                OutsideOfCueBall

        Nothing ->
            OutsideOfCueBall


mouseOrbiting : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Model -> Model
mouseOrbiting originalPosition newPosition model =
    let
        ( deltaX, deltaY ) =
            newPosition
                |> Vector2d.from originalPosition
                |> Vector2d.components

        radiansInPixels =
            orbitingPrecision model.zoom

        newAzimuth =
            angleFromTimeline model.azimuth
                |> Quantity.minus (Quantity.at radiansInPixels deltaX)
                |> Angle.normalize

        newElevation =
            angleFromTimeline model.elevation
                |> Quantity.plus (Quantity.at radiansInPixels deltaY)
                |> Quantity.clamp (Angle.degrees 6) (Angle.degrees 90)
    in
    { model
        | orbiting = Just newPosition
        , azimuth = Animator.go Animator.immediately newAzimuth model.azimuth
        , elevation = Animator.go Animator.immediately newElevation model.elevation
    }


setCueElevation : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Timeline Float -> PlayingState -> PlayingState
setCueElevation originalPosition newPosition zoomTimeline playingState =
    let
        deltaElevation =
            Vector2d.from originalPosition newPosition
                |> Vector2d.yComponent
                |> Quantity.at (orbitingPrecision zoomTimeline)
    in
    { playingState
        | cueElevation =
            playingState.cueElevation
                |> Quantity.minus deltaElevation
                |> Quantity.clamp (Angle.degrees 0) (Angle.degrees 90)
    }


{-| Apply impulse to the cue ball depending on the shooting strength.
The strength is calculated based on how long the spacebar has been pressed.
-}
shoot : Axis3d Meters WorldCoordinates -> Posix -> Posix -> Bool -> World Id -> World Id
shoot axis startTime endTime isBreak =
    World.update
        (\body ->
            if Body.data body == CueBall then
                let
                    shootingAxis =
                        Axis3d.reverse axis

                    force =
                        Quantity.interpolateFrom
                            (Force.newtons 10)
                            (if isBreak then
                                -- Make break a bit stronger
                                Force.newtons 100

                             else
                                Force.newtons 60
                            )
                            (shootingStrength startTime endTime)
                in
                Body.applyImpulse
                    (Quantity.times (Duration.milliseconds 16) force)
                    (Axis3d.direction shootingAxis)
                    (Axis3d.originPoint shootingAxis)
                    body

            else
                body
        )


{-| Returns a value from 0 to 1
-}
shootingStrength : Posix -> Posix -> Float
shootingStrength startTime endTime =
    let
        duration =
            toFloat (Time.posixToMillis endTime - Time.posixToMillis startTime)
    in
    -(cos (duration / 2000 * pi) / 2) + 0.5


canSpawnHere : Axis3d Meters WorldCoordinates -> Rectangle3d Meters WorldCoordinates -> World Id -> PlacingBallMouse
canSpawnHere mouseRay spawnArea world =
    let
        hoveringTable =
            world
                |> World.keepIf (\b -> Body.data b /= Floor)
                |> World.raycast mouseRay
                |> (/=) Nothing

        planeIntersection =
            Axis3d.intersectionWithPlane Plane3d.xy mouseRay
    in
    -- TODO: rather use a Rectangle3d that defines the table surface
    case ( hoveringTable, planeIntersection ) of
        ( True, Just point1 ) ->
            case Axis3d.intersectionWithRectangle spawnArea mouseRay of
                Just point2 ->
                    let
                        position =
                            Point3d.translateIn Direction3d.z (Length.millimeters (57.15 / 2)) point2

                        canSpawn =
                            List.all
                                (\b ->
                                    case Body.data b of
                                        Numbered _ ->
                                            Quantity.greaterThan (Length.millimeters 57.15)
                                                (Point3d.distanceFrom position (Body.originPoint b))

                                        _ ->
                                            True
                                )
                                (World.bodies world)
                    in
                    if canSpawn then
                        CanSpawnAt position

                    else
                        CannotSpawn position

                Nothing ->
                    CannotSpawn (Point3d.translateIn Direction3d.z (Length.millimeters (57.15 / 2)) point1)

        _ ->
            HoveringOuside


{-| Find the frozen balls, that are touching the walls
-}
frozenBalls : World Id -> Set Int
frozenBalls world =
    List.foldl
        (\contact frozen ->
            let
                ( b1, b2 ) =
                    Contact.bodies contact
            in
            case ( Body.data b1, Body.data b2 ) of
                ( Walls, Numbered ball ) ->
                    Set.insert (EightBall.ballNumber ball) frozen

                ( Numbered ball, Walls ) ->
                    Set.insert (EightBall.ballNumber ball) frozen

                _ ->
                    frozen
        )
        Set.empty
        (World.contacts world)


{-| Find out if the cue ball is touching the wall.
-}
frozenCueBall : World Id -> Bool
frozenCueBall world =
    List.any
        (\contact ->
            let
                ( b1, b2 ) =
                    Contact.bodies contact
            in
            case ( Body.data b1, Body.data b2 ) of
                ( Walls, CueBall ) ->
                    True

                ( CueBall, Walls ) ->
                    True

                _ ->
                    False
        )
        (World.contacts world)


simulateWithEvents : Int -> Time.Posix -> World Id -> List ( Time.Posix, ShotEvent ) -> ( World Id, List ( Time.Posix, ShotEvent ) )
simulateWithEvents frame time world events =
    if frame > 0 then
        let
            frozen =
                -- Frozen balls from before the simulation
                frozenBalls world

            frozenCue =
                -- Frozen cue ball from before the simulation
                frozenCueBall world

            simulatedWorld =
                -- Simulate at shorter interval to prevent tunneling
                World.simulate (Duration.seconds (1 / 120)) world

            contacts =
                World.contacts simulatedWorld

            ( newEvents, newWorld ) =
                List.foldl
                    (\contact ( currentEvents, currentWorld ) ->
                        let
                            ( b1, b2 ) =
                                Contact.bodies contact
                        in
                        case ( Body.data b1, Body.data b2 ) of
                            -- TODO: check collisions with pockets instead when we have them
                            ( Numbered ball, Floor ) ->
                                ( EightBall.ballFellInPocket time ball :: currentEvents
                                , World.keepIf (\b -> Body.data b /= Numbered ball) currentWorld
                                )

                            ( Floor, Numbered ball ) ->
                                ( EightBall.ballFellInPocket time ball :: currentEvents
                                , World.keepIf (\b -> Body.data b /= Numbered ball) currentWorld
                                )

                            ( CueBall, Numbered ball ) ->
                                ( EightBall.cueHitBall time ball :: currentEvents, currentWorld )

                            ( Numbered ball, CueBall ) ->
                                ( EightBall.cueHitBall time ball :: currentEvents, currentWorld )

                            ( CueBall, Floor ) ->
                                ( EightBall.scratch time :: currentEvents
                                , World.keepIf (\b -> Body.data b /= CueBall) currentWorld
                                )

                            ( Floor, CueBall ) ->
                                ( EightBall.scratch time :: currentEvents
                                , World.keepIf (\b -> Body.data b /= CueBall) currentWorld
                                )

                            --(Numbered _, Numbered _) ->
                            --    (EightBall.ballsCollided time, currentWorld)
                            ( Walls, Numbered ball ) ->
                                if not (Set.member (EightBall.ballNumber ball) frozen) then
                                    ( EightBall.ballHitWall time ball :: currentEvents, currentWorld )

                                else
                                    ( currentEvents, currentWorld )

                            ( Numbered ball, Walls ) ->
                                if not (Set.member (EightBall.ballNumber ball) frozen) then
                                    ( EightBall.ballHitWall time ball :: currentEvents, currentWorld )

                                else
                                    ( currentEvents, currentWorld )

                            ( Walls, CueBall ) ->
                                if not frozenCue then
                                    ( EightBall.cueHitWall time :: currentEvents, currentWorld )

                                else
                                    ( currentEvents, currentWorld )

                            ( CueBall, Walls ) ->
                                if not frozenCue then
                                    ( EightBall.cueHitWall time :: currentEvents, currentWorld )

                                else
                                    ( currentEvents, currentWorld )

                            _ ->
                                ( currentEvents, currentWorld )
                    )
                    ( events, simulatedWorld )
                    contacts
        in
        simulateWithEvents (frame - 1) time newWorld newEvents

    else
        ( world, events )


{-| Read the angle value from the timeline
-}
angleFromTimeline : Timeline Angle -> Angle
angleFromTimeline angleTimeline =
    Angle.radians (Animator.linear angleTimeline (Angle.inRadians >> Animator.at))


{-| Make orbiting precision depend on zoom level.
Controls how much radians correspond to the change in mouse offset.
-}
orbitingPrecision : Timeline Float -> Quantity Float (Quantity.Rate Angle.Radians Pixels)
orbitingPrecision zoomTimeline =
    Quantity.rate
        (Angle.radians (0.2 + Animator.linear zoomTimeline Animator.at / 0.8))
        (Pixels.pixels (180 / pi))


decodeMouse : (Point2d Pixels ScreenCoordinates -> Msg) -> Json.Decode.Decoder Msg
decodeMouse msg =
    Json.Decode.map2 (\x y -> msg (Point2d.pixels x y))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


decodeKey : Msg -> Json.Decode.Decoder Msg
decodeKey msg =
    Json.Decode.andThen
        (\key ->
            if key == " " then
                Json.Decode.succeed msg

            else
                Json.Decode.fail ""
        )
        (Json.Decode.field "key" Json.Decode.string)
