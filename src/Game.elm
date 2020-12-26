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
import Geometry
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Length, Meters)
import Physics.Body as Body
import Physics.Contact as Contact
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material
import SketchPlane3d
import Speed
import Time exposing (Posix)
import Vector2d
import Vector3d
import Viewpoint3d


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { world : World Id
    , ballTextures : Dict Int (Material.Texture Color)
    , roughnessTexture : Material.Texture Float
    , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
    , distance : Length
    , focalPoint : Point3d Meters WorldCoordinates
    , cameraAzimuth : Angle
    , cameraElevation : Angle
    , orbiting : Maybe (Point2d Pixels ScreenCoordinates)
    , state : State
    , time : Posix
    }


type State
    = PlacingBehindHeadString (PlacingBallState AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingState
    | Simulating (List ( Time.Posix, ShotEvent )) (Pool AwaitingNextShot)
    | PlacingBallInHand (PlacingBallState AwaitingBallInHand)
    | GameOver (EightBall.Pool AwaitingNewGame) Int


type alias PlayingState =
    { pool : Pool AwaitingNextShot
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


initialPlayingState : Pool AwaitingNextShot -> State
initialPlayingState pool =
    Playing
        { pool = pool
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
    , focalPoint = Point3d.origin
    , cameraAzimuth = Angle.degrees -25
    , cameraElevation = Angle.degrees 30
    , orbiting = Nothing
    , state = initialPlacingBallState PlacingBehindHeadString (EightBall.rack time EightBall.start)
    }


camera : Length -> Angle -> Angle -> Point3d Meters WorldCoordinates -> Camera3d Meters WorldCoordinates
camera distance cameraAzimuth cameraElevation focalPoint =
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


cuePosition : World Id -> Point3d Meters WorldCoordinates
cuePosition world =
    World.bodies world
        |> List.filter (\b -> Body.data b == CueBall)
        |> List.head
        |> Maybe.map (\b -> Point3d.placeIn (Body.frame b) (Body.centerOfMass b))
        |> Maybe.withDefault Point3d.origin


ray : Model -> Point2d Pixels ScreenCoordinates -> Axis3d Meters WorldCoordinates
ray { dimensions, distance, cameraAzimuth, cameraElevation, focalPoint } =
    Camera3d.ray
        (camera distance cameraAzimuth cameraElevation focalPoint)
        (Rectangle2d.with
            { x1 = pixels 0
            , y1 = Tuple.second dimensions
            , x2 = Tuple.first dimensions
            , y2 = pixels 0
            }
        )


view : Model -> Html Msg
view ({ world, ballTextures, roughnessTexture, dimensions, distance, cameraAzimuth, cameraElevation, focalPoint } as model) =
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
                    placingBallEntities mouse Bodies.areaBallInHandEntity :: entities

                Playing playingState ->
                    playingEntities model playingState :: entities

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
            , camera = camera distance cameraAzimuth cameraElevation focalPoint
            , entities = entitiesWithUI
            , lights = Scene3d.twoLights environmentalLighting sunlight
            , exposure = Scene3d.exposureValue 13
            , whiteBalance = Scene3d.Light.daylight
            , clipDepth = Length.meters 0.1
            , background = Scene3d.backgroundColor Color.black
            , toneMapping = Scene3d.noToneMapping
            }
        , viewCurrentStatus model.state
        ]


cueAxis : Model -> PlayingState -> Axis3d Meters WorldCoordinates
cueAxis { cameraAzimuth, world } { hitRelativeAzimuth, cueElevation, hitElevation } =
    let
        hitAzimuth =
            cameraAzimuth
                |> Quantity.plus hitRelativeAzimuth

        pointDirection =
            Direction3d.xyZ hitAzimuth hitElevation

        point =
            cuePosition world
                |> Point3d.translateIn pointDirection (Length.millimeters (57.15 / 2))

        axisDirection =
            Direction3d.xyZ cameraAzimuth cueElevation
    in
    Axis3d.through point axisDirection


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


playingEntities : Model -> PlayingState -> Scene3d.Entity WorldCoordinates
playingEntities model playingState =
    let
        axis =
            cueAxis model playingState

        maybeCylinder =
            Cylinder3d.from
                (Point3d.along axis (Length.centimeters 2))
                (Point3d.along axis (Length.centimeters (2 + 150)))
                (Length.millimeters 6)
    in
    case maybeCylinder of
        Just cylinder ->
            Scene3d.cylinderWithShadow
                (Material.nonmetal
                    { baseColor = Color.rgb255 255 255 255
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
    font-display: swap;
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


currentPlayer : State -> String
currentPlayer state =
    let
        playerIndex =
            case state of
                PlacingBehindHeadString { pool } ->
                    EightBall.currentPlayer pool

                Playing { pool } ->
                    EightBall.currentPlayer pool

                Simulating _ pool ->
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

                Simulating _ pool ->
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
                                            (Quantity.plus (seconds (1 / 120)))
                                            playingState.shootButton
                                }
                    }

                Simulating events pool ->
                    if ballsStoppedMoving newModel.world then
                        case EightBall.playerShot (List.reverse events) pool of
                            PlayersFault newPool ->
                                { newModel
                                    | state = initialPlacingBallState PlacingBallInHand newPool
                                    , focalPoint = Point3d.origin
                                }

                            NextShot newPool ->
                                { newModel
                                    | state = initialPlayingState newPool
                                    , focalPoint = cuePosition newModel.world
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
                                | focalPoint = position
                                , state = initialPlayingState (EightBall.ballPlacedInHand model.time pool)
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
                                | focalPoint = position
                                , state = initialPlayingState (EightBall.ballPlacedBehindHeadString model.time pool)
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

                Simulating _ _ ->
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
                    let
                        newPlayingState =
                            { playingState | shootButton = Just (Duration.milliseconds 0) }
                    in
                    { model | state = Playing newPlayingState }

                _ ->
                    model

        ShootButtonUp ->
            case model.state of
                Playing playingState ->
                    { model
                        | state = Simulating [] playingState.pool
                        , focalPoint = Point3d.origin
                        , world =
                            World.update
                                (\b ->
                                    if Body.data b == CueBall then
                                        let
                                            axis =
                                                cueAxis model playingState
                                        in
                                        -- TODO use the duration from the Pressed state to calculate the force
                                        Body.applyImpulse
                                            (Quantity.times (Duration.milliseconds 16) (Force.newtons 50))
                                            (Axis3d.reverse axis |> Axis3d.direction)
                                            (Axis3d.originPoint axis)
                                            b

                                    else
                                        b
                                )
                                model.world
                    }

                _ ->
                    model

        StartNewGameButtonClicked ->
            initial model.ballTextures
                model.roughnessTexture
                (Tuple.mapBoth Quantity.unwrap
                    Quantity.unwrap
                    model.dimensions
                )


canSpawnHere : Axis3d Meters WorldCoordinates -> Rectangle3d Meters WorldCoordinates -> World Id -> PlacingBallMouse
canSpawnHere mouseRay area world =
    case Geometry.intersectionWithRectangle mouseRay Bodies.areaBallInHand of
        Just point1 ->
            case Geometry.intersectionWithRectangle mouseRay area of
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
