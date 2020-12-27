module Game exposing (Model, Msg, initial, subscriptions, update, view)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Bodies exposing (Id(..))
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration, seconds)
import EightBall exposing (AwaitingBallInHand, AwaitingNewGame, AwaitingNextShot, AwaitingPlaceBallBehindHeadstring, Pool, ShotEvent, WhatHappened(..))
import Force
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Length, Meters)
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
import Rectangle3d exposing (Rectangle3d, dimensions)
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material
import SketchPlane3d
import Speed
import Time exposing (Posix)
import Vector2d
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { world : World Id
    , ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Material.Texture Float
    , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
    , distance : Length
    , cameraAzimuth : Angle
    , cameraElevation : Angle
    , orbiting : Maybe (Point2d Pixels ScreenCoordinates)
    , state : State
    , time : Posix
    }


type State
    = PlacingBehindHeadString (PlacingBallState AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingState
    | Simulating SimulatingState
    | PlacingBallInHand (PlacingBallState AwaitingBallInHand)
    | GameOver (EightBall.Pool AwaitingNewGame) Int


type alias PlayingState =
    { pool : Pool AwaitingNextShot
    , cueBallPosition : Point3d Meters WorldCoordinates
    , cueElevation : Angle
    , hitElevation : Angle
    , hitRelativeAzimuth : Angle
    , shootButton : Maybe Duration
    , mouse : PlayingMouse
    }


type PlayingMouse
    = HoveringCueBall -- TODO: use to render the clickable area
    | SettingCueElevation (Point2d Pixels ScreenCoordinates)
    | NothingMeaningful


initialPlayingState : Point3d Meters WorldCoordinates -> Pool AwaitingNextShot -> State
initialPlayingState cueBallPosition pool =
    Playing
        { pool = pool
        , cueBallPosition = cueBallPosition
        , cueElevation = Angle.degrees 5
        , hitRelativeAzimuth = Angle.degrees 0
        , hitElevation = Angle.degrees 0
        , mouse = NothingMeaningful
        , shootButton = Nothing
        }


type alias PlacingBallState awaitingWhat =
    { pool : Pool awaitingWhat
    , mouse : PlacingBallMouse
    }


type PlacingBallMouse
    = CanSpawnAt (Point3d Meters WorldCoordinates)
    | CannotSpawn (Point3d Meters WorldCoordinates)
    | HoveringOuside


initialPlacingBallState : (PlacingBallState awaitingWhat -> State) -> Pool awaitingWhat -> State
initialPlacingBallState fn pool =
    fn
        { pool = pool
        , mouse = HoveringOuside
        }


type alias SimulatingState =
    { cueBallPosition : Point3d Meters WorldCoordinates
    , events : List ( Time.Posix, ShotEvent )
    , pool : Pool AwaitingNextShot
    }


initialSimulatingState : Point3d Meters WorldCoordinates -> Pool AwaitingNextShot -> State
initialSimulatingState cueBallPosition pool =
    Simulating
        { cueBallPosition = cueBallPosition
        , pool = pool
        , events = []
        }


type Msg
    = Tick Time.Posix
    | Resize Int Int
    | MouseWheel Float
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | ShootButtonDown
    | ShootButtonUp
    | StartNewGameButtonClicked


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
    , distance = Length.meters 4
    , cameraAzimuth = Angle.degrees -25
    , cameraElevation = Angle.degrees 30
    , orbiting = Nothing
    , state = initialPlacingBallState PlacingBehindHeadString (EightBall.rack time EightBall.start)
    }


camera : Length -> Angle -> Angle -> State -> Camera3d Meters WorldCoordinates
camera distance cameraAzimuth cameraElevation state =
    let
        focalPoint =
            case state of
                Playing { cueBallPosition } ->
                    cueBallPosition

                Simulating { cueBallPosition } ->
                    -- TODO: animate to Point3d.origin
                    cueBallPosition

                _ ->
                    Point3d.origin
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = focalPoint
                , groundPlane = SketchPlane3d.xy
                , azimuth = cameraAzimuth
                , elevation = cameraElevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


ray : Model -> Point2d Pixels ScreenCoordinates -> Axis3d Meters WorldCoordinates
ray { dimensions, distance, cameraAzimuth, cameraElevation, state } =
    Camera3d.ray
        (camera distance cameraAzimuth cameraElevation state)
        (Rectangle2d.with
            { x1 = pixels 0
            , y1 = Tuple.second dimensions
            , x2 = Tuple.first dimensions
            , y2 = pixels 0
            }
        )


view : Model -> Html Msg
view ({ world, ballTextures, roughnessTexture, dimensions, distance, cameraAzimuth, cameraElevation } as model) =
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
            camera distance cameraAzimuth cameraElevation model.state

        bodies =
            case model.state of
                PlacingBehindHeadString _ ->
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
                PlacingBehindHeadString { mouse } ->
                    placingBallEntities mouse Bodies.areaBehindTheHeadStringEntity :: entities

                PlacingBallInHand { mouse } ->
                    placingBallEntities mouse Scene3d.nothing :: entities

                Playing playingState ->
                    playingEntities world playingState camera3d model.cameraAzimuth :: entities

                _ ->
                    entities

        cursor =
            case model.state of
                PlacingBehindHeadString { mouse } ->
                    if mouse == HoveringOuside then
                        "default"

                    else
                        "none"

                PlacingBallInHand { mouse } ->
                    if mouse == HoveringOuside then
                        "default"

                    else
                        "none"

                Playing { mouse } ->
                    case mouse of
                        HoveringCueBall ->
                            "pointer"

                        SettingCueElevation _ ->
                            "ns-resize"

                        _ ->
                            "default"

                Simulating _ ->
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
        , viewCurrentStatus model.state
        , viewShootingStrength model
        ]


cueAxis : PlayingState -> Angle -> Axis3d Meters WorldCoordinates
cueAxis { hitRelativeAzimuth, cueElevation, cueBallPosition, hitElevation } cameraAzimuth =
    let
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
    in
    List.all
        (\n ->
            let
                angle =
                    Angle.degrees (360 * toFloat n / 8)

                origin =
                    Point3d.rotateAround axis angle pointOnCue

                cueRay =
                    Axis3d.through origin direction

                worldWithoutCueBall =
                    World.keepIf (\b -> Body.data b /= CueBall) world
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
                    (Material.matte (Color.rgb255 255 150 150))
                    Bodies.ballSphere
                    |> Scene3d.placeIn (Frame3d.atPoint position)
                ]

        HoveringOuside ->
            Scene3d.nothing


playingEntities : World Id -> PlayingState -> Camera3d Meters WorldCoordinates -> Angle -> Scene3d.Entity WorldCoordinates
playingEntities world playingState camera3d cameraAzimuth =
    let
        axis =
            cueAxis playingState cameraAzimuth

        viewpoint =
            Camera3d.viewpoint camera3d

        viewPlane =
            SketchPlane3d.toPlane (Viewpoint3d.viewPlane viewpoint)

        cueMaxDistance =
            Length.centimeters (2 + 150)

        cueRadius =
            Length.millimeters 6

        cueDistance =
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
                            Color.rgb255 255 255 255

                        else
                            Color.rgb255 255 150 150
                    , roughness = 0.6
                    }
                )
                cylinder

        Nothing ->
            Scene3d.nothing



-- View Current Status


viewCurrentStatus : State -> Html Msg
viewCurrentStatus state =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "bottom" "0"
        , Html.Attributes.style "left" "50%" -- To center absolute content.
        ]
        [ Html.node "style"
            []
            [ Html.text """
@font-face {
    font-family: 'Teko';
    src: url('assets/Teko-Medium.woff2') format('woff2'),
        url('assets/Teko-Medium.woff') format('woff');
    font-weight: 500;
    font-style: normal;
    font-display: block;
}
"""
            ]
        , case state of
            GameOver _ _ ->
                viewGameOver state

            _ ->
                Html.div statusStyle
                    [ Html.text (currentPlayer state)
                    , Html.text " - "
                    , Html.text (currentTarget state)
                    ]
        ]


statusStyle : List (Html.Attribute Msg)
statusStyle =
    [ -- To center within absolute container.
      Html.Attributes.style "position" "relative"
    , Html.Attributes.style "left" "-50%"
    , Html.Attributes.style "text-align" "center"

    -- Color
    , Html.Attributes.style "background-color" "#121517"
    , Html.Attributes.style "color" "#eef"

    -- Border/Edges
    , Html.Attributes.style "border-radius" "10px 10px 0 0"
    , Html.Attributes.style "box-shadow" "0 4px 12px 0 rgba(0, 0, 0, 0.15)"

    -- Font
    , Html.Attributes.style "font-family" "Teko"
    , Html.Attributes.style "font-size" "40px"

    -- Other
    , Html.Attributes.style "opacity" "0.9"
    , Html.Attributes.style "padding" "15px 25px 10px 25px"
    ]


viewGameOver : State -> Html Msg
viewGameOver state =
    Html.button
        (statusStyle
            ++ [ Html.Attributes.style "border" "none"
               , Html.Attributes.style "cursor" "pointer"
               , Html.Events.onClick StartNewGameButtonClicked
               ]
        )
        [ Html.text (currentPlayer state)
        , Html.text " won!"
        , Html.div
            [ Html.Attributes.style "color" "rgba(86, 186, 79, 0.7)"
            ]
            [ Html.text "Play again?"
            ]
        ]


viewShootingStrength : Model -> Html Msg
viewShootingStrength { state, dimensions } =
    case state of
        Playing { shootButton } ->
            case shootButton of
                Nothing ->
                    Html.text ""

                Just duration ->
                    let
                        progressHeight =
                            shootingStrength duration * (barHeight - 4)

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


currentPlayer : State -> String
currentPlayer state =
    let
        playerIndex =
            case state of
                PlacingBehindHeadString { pool } ->
                    EightBall.currentPlayer pool

                Playing { pool } ->
                    EightBall.currentPlayer pool

                Simulating { pool } ->
                    EightBall.currentPlayer pool

                PlacingBallInHand { pool } ->
                    EightBall.currentPlayer pool

                GameOver _ winner ->
                    winner
    in
    case playerIndex of
        0 ->
            "Player 1"

        1 ->
            "Player 2"

        _ ->
            "Unknown"


currentTarget : State -> String
currentTarget state =
    let
        currentTarget_ : EightBall.CurrentTarget
        currentTarget_ =
            case state of
                PlacingBehindHeadString { pool } ->
                    EightBall.currentTarget pool

                Playing { pool } ->
                    EightBall.currentTarget pool

                Simulating { pool } ->
                    EightBall.currentTarget pool

                PlacingBallInHand { pool } ->
                    EightBall.currentTarget pool

                GameOver pool _ ->
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
                newModel =
                    { model | time = time }
            in
            case newModel.state of
                Playing playingState ->
                    { newModel
                        | state =
                            Playing
                                { playingState
                                    | shootButton =
                                        Maybe.map
                                            (Quantity.plus (seconds (1 / 60)))
                                            playingState.shootButton
                                }
                    }

                Simulating simulatingState ->
                    if ballsStoppedMoving newModel.world then
                        case EightBall.playerShot (List.reverse simulatingState.events) simulatingState.pool of
                            PlayersFault newPool ->
                                { newModel
                                    | state = initialPlacingBallState PlacingBallInHand newPool
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
                                    | state = initialPlayingState cuePosition newPool
                                }

                            EightBall.GameOver newPool { winner } ->
                                { newModel
                                    | state = GameOver newPool winner
                                }

                            Error _ ->
                                Debug.todo "Error"

                    else
                        let
                            ( newWorld, newEvents ) =
                                simulateWithEvents 2 time newModel.world simulatingState.events

                            newSimulatingState =
                                { simulatingState | events = newEvents }
                        in
                        { newModel
                            | state = Simulating newSimulatingState
                            , world = newWorld
                        }

                _ ->
                    newModel

        Resize width height ->
            { model | dimensions = ( Pixels.float (toFloat width), Pixels.float (toFloat height) ) }

        MouseWheel deltaY ->
            { model
                | distance =
                    model.distance
                        |> Quantity.minus (Length.meters (deltaY * 0.01))
                        |> Quantity.clamp (Length.meters 0.5) (Length.meters 5)
            }

        MouseDown mouse ->
            case model.state of
                PlacingBallInHand { pool } ->
                    case canSpawnHere (ray model mouse) Bodies.areaBallInHand model.world of
                        CanSpawnAt position ->
                            { model
                                | state = initialPlayingState position (EightBall.ballPlacedInHand model.time pool)
                                , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                            }

                        CannotSpawn _ ->
                            model

                        HoveringOuside ->
                            { model | orbiting = Just mouse }

                PlacingBehindHeadString { pool } ->
                    case canSpawnHere (ray model mouse) Bodies.areaBehindTheHeadString model.world of
                        CanSpawnAt position ->
                            { model
                                | state = initialPlayingState position (EightBall.ballPlacedBehindHeadString model.time pool)
                                , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                            }

                        CannotSpawn _ ->
                            model

                        HoveringOuside ->
                            { model | orbiting = Just mouse }

                Playing playingState ->
                    case World.raycast (ray model mouse) model.world of
                        Just raycastResult ->
                            case Body.data raycastResult.body of
                                CueBall ->
                                    let
                                        frame =
                                            Body.frame raycastResult.body

                                        normal =
                                            Direction3d.placeIn frame raycastResult.normal

                                        hitAzimuth =
                                            Direction3d.azimuthIn SketchPlane3d.xy normal

                                        hitElevation =
                                            Direction3d.elevationFrom SketchPlane3d.xy normal

                                        hitRelativeAzimuth =
                                            Quantity.minus model.cameraAzimuth hitAzimuth
                                                |> Angle.normalize

                                        hitRelativeAzimuthDegrees =
                                            Angle.inDegrees hitRelativeAzimuth
                                    in
                                    -- Can only click on the visible hemisphere
                                    if abs hitRelativeAzimuthDegrees < 90 then
                                        { model
                                            | state =
                                                Playing
                                                    { playingState
                                                        | mouse = SettingCueElevation mouse
                                                        , hitElevation = hitElevation
                                                        , hitRelativeAzimuth = hitRelativeAzimuth
                                                    }
                                        }

                                    else
                                        { model | orbiting = Just mouse }

                                _ ->
                                    { model | orbiting = Just mouse }

                        Nothing ->
                            { model | orbiting = Just mouse }

                Simulating _ ->
                    { model | orbiting = Just mouse }

                GameOver _ _ ->
                    model

        MouseMove mouse ->
            case model.orbiting of
                Just from ->
                    let
                        { x, y } =
                            Vector2d.toPixels (Vector2d.from from mouse)

                        cameraAzimuth =
                            model.cameraAzimuth
                                |> Quantity.minus (Angle.degrees x)
                                |> Angle.normalize

                        cameraElevation =
                            model.cameraElevation
                                |> Quantity.plus (Angle.degrees y)
                                |> Quantity.clamp
                                    (Angle.degrees 6)
                                    (Angle.degrees 90)
                    in
                    { model
                        | orbiting = Just mouse
                        , cameraAzimuth = cameraAzimuth
                        , cameraElevation = cameraElevation
                    }

                Nothing ->
                    case model.state of
                        PlacingBallInHand { pool } ->
                            let
                                newPlacingState =
                                    { pool = pool
                                    , mouse = canSpawnHere (ray model mouse) Bodies.areaBallInHand model.world
                                    }
                            in
                            { model | state = PlacingBallInHand newPlacingState }

                        PlacingBehindHeadString { pool } ->
                            let
                                newPlacingState =
                                    { pool = pool
                                    , mouse = canSpawnHere (ray model mouse) Bodies.areaBehindTheHeadString model.world
                                    }
                            in
                            { model | state = PlacingBehindHeadString newPlacingState }

                        Playing playingState ->
                            case playingState.mouse of
                                SettingCueElevation point ->
                                    let
                                        { y } =
                                            Vector2d.toPixels (Vector2d.from point mouse)

                                        newPlayingState =
                                            { playingState
                                                | mouse = SettingCueElevation mouse
                                                , cueElevation =
                                                    playingState.cueElevation
                                                        |> Quantity.minus (Angle.degrees y)
                                                        |> Quantity.clamp (Angle.degrees 0) (Angle.degrees 90)
                                            }
                                    in
                                    { model | state = Playing newPlayingState }

                                _ ->
                                    case World.raycast (ray model mouse) model.world of
                                        Just raycastResult ->
                                            case Body.data raycastResult.body of
                                                CueBall ->
                                                    { model | state = Playing { playingState | mouse = HoveringCueBall } }

                                                _ ->
                                                    { model | state = Playing { playingState | mouse = NothingMeaningful } }

                                        Nothing ->
                                            { model | state = Playing { playingState | mouse = NothingMeaningful } }

                        _ ->
                            model

        MouseUp ->
            case model.state of
                Playing playingState ->
                    let
                        newPlayingState =
                            { playingState | mouse = NothingMeaningful }
                    in
                    { model
                        | state = Playing newPlayingState
                        , orbiting = Nothing
                    }

                _ ->
                    { model | orbiting = Nothing }

        ShootButtonDown ->
            case model.state of
                Playing playingState ->
                    if canShoot (cueAxis playingState model.cameraAzimuth) model.world then
                        -- ShootButtonDown can be sent many times
                        -- we need to check if it wasn't already pressed
                        case playingState.shootButton of
                            Nothing ->
                                let
                                    newPlayingState =
                                        { playingState
                                            | shootButton =
                                                Just (Duration.milliseconds 0)
                                        }
                                in
                                { model | state = Playing newPlayingState }

                            _ ->
                                model

                    else
                        model

                _ ->
                    model

        ShootButtonUp ->
            case model.state of
                Playing playingState ->
                    if canShoot (cueAxis playingState model.cameraAzimuth) model.world then
                        case playingState.shootButton of
                            Just duration ->
                                { model
                                    | state = initialSimulatingState playingState.cueBallPosition playingState.pool
                                    , world =
                                        World.update
                                            (\b ->
                                                if Body.data b == CueBall then
                                                    let
                                                        axis =
                                                            cueAxis playingState model.cameraAzimuth

                                                        force =
                                                            Quantity.interpolateFrom
                                                                (Force.newtons 10)
                                                                (Force.newtons 60)
                                                                (shootingStrength duration)
                                                    in
                                                    Body.applyImpulse
                                                        (Quantity.times (Duration.milliseconds 16) force)
                                                        (Axis3d.reverse axis |> Axis3d.direction)
                                                        (Axis3d.originPoint axis)
                                                        b

                                                else
                                                    b
                                            )
                                            model.world
                                }

                            Nothing ->
                                model

                    else
                        { model | state = Playing { playingState | shootButton = Nothing } }

                _ ->
                    model

        StartNewGameButtonClicked ->
            initial model.ballTextures
                model.roughnessTexture
                (Tuple.mapBoth Quantity.unwrap
                    Quantity.unwrap
                    model.dimensions
                )


shootingStrength : Duration -> Float
shootingStrength duration =
    let
        ms =
            Duration.inMilliseconds duration
    in
    -(cos (ms / 2000 * pi) / 2) + 0.5


canSpawnHere : Axis3d Meters WorldCoordinates -> Rectangle3d Meters WorldCoordinates -> World Id -> PlacingBallMouse
canSpawnHere mouseRay area world =
    let
        hitsTable =
            case World.raycast mouseRay world of
                Just { body } ->
                    Body.data body /= Floor

                Nothing ->
                    False
    in
    if hitsTable then
        case Axis3d.intersectionWithPlane Plane3d.xy mouseRay of
            Just point1 ->
                case intersectionWithRectangle mouseRay area of
                    Just point2 ->
                        let
                            position =
                                Point3d.translateBy (Vector3d.millimeters 0 0 (57.15 / 2)) point2

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
                        CannotSpawn (Point3d.translateBy (Vector3d.millimeters 0 0 (57.15 / 2)) point1)

            Nothing ->
                HoveringOuside

    else
        HoveringOuside


simulateWithEvents : Int -> Time.Posix -> World Id -> List ( Time.Posix, ShotEvent ) -> ( World Id, List ( Time.Posix, ShotEvent ) )
simulateWithEvents frame time world events =
    if frame > 0 then
        let
            simulatedWorld =
                -- Simulate at shorter interval to prevent tunneling
                World.simulate (seconds (1 / 120)) world

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
                            --    (EightBall.twoBallsCollided time, currentWorld)
                            --( Walls, Numbered _ ) ->
                            --    (EightBall.ballTouchedTheWall time, currentWorld)
                            --( Numbered _, Walls ) ->
                            --    (EightBall.ballTouchedTheWall time, currentWorld)
                            _ ->
                                ( currentEvents, currentWorld )
                    )
                    ( events, simulatedWorld )
                    (World.contacts simulatedWorld)
        in
        simulateWithEvents (frame - 1) time newWorld newEvents

    else
        ( world, events )


intersectionWithRectangle : Axis3d units coordinates -> Rectangle3d units coordinates -> Maybe (Point3d units coordinates)
intersectionWithRectangle axis rectangle =
    let
        plane =
            Rectangle3d.axes rectangle
    in
    case Axis3d.intersectionWithPlane (SketchPlane3d.toPlane plane) axis of
        (Just point) as result ->
            let
                ( width, height ) =
                    Rectangle3d.dimensions rectangle

                ( x, y ) =
                    Point2d.coordinates (Point3d.projectInto plane point)
            in
            if
                Quantity.lessThanOrEqualTo (Quantity.half width) (Quantity.abs x)
                    && Quantity.lessThanOrEqualTo (Quantity.half height) (Quantity.abs y)
            then
                result

            else
                Nothing

        Nothing ->
            Nothing


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
