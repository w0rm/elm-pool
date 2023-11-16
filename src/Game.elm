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
import Dict exposing (Dict)
import Direction3d
import Duration
import EightBall exposing (Player, Pool, ShotEvent)
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
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d
import Scene3d.Light
import Scene3d.Material exposing (Texture)
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
    , ballTextures : Dict Int (Texture Color)
    , roughnessTexture : Texture Float
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
    = PlacingBall BallInHand PoolWithBallInHand
    | Playing PlayingMouse Cue (Pool EightBall.AwaitingPlayerShot)
    | Simulating (List ( Posix, ShotEvent )) (Pool EightBall.AwaitingPlayerShot)
    | GameOver Player (Pool EightBall.AwaitingStart)


type PoolWithBallInHand
    = BehindHeadString (Pool EightBall.AwaitingPlaceBallBehindHeadstring)
    | Anywhere (Pool EightBall.AwaitingPlaceBallInHand)


type alias Cue =
    { elevation : Angle
    , shootPressedAt : Maybe Posix

    -- polar coordinates of the hit point on the surface of the cue ball
    , hitRelativeAzimuth : Angle -- relative to the camera azimuth
    , hitElevation : Angle
    }


initialCue : Cue
initialCue =
    { elevation = Angle.degrees 5
    , shootPressedAt = Nothing
    , hitRelativeAzimuth = Angle.degrees 0
    , hitElevation = Angle.degrees 0
    }


type PlayingMouse
    = HoveringCueBall { hitRelativeAzimuth : Angle, hitElevation : Angle }
    | ElevatingCue (Point2d Pixels ScreenCoordinates)
    | OutsideOfCueBall


type BallInHand
    = OnTable CanPlace (Point3d Meters WorldCoordinates)
    | OutsideOfTable


type CanPlace
    = CanPlace
    | CannotPlace


type Msg
    = Tick Posix
    | Resize Int Int
    | MouseWheel Float
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | ShootPressed
    | ShootReleased


initial : Dict Int (Texture Color) -> Texture Float -> ( Quantity Float Pixels, Quantity Float Pixels ) -> Model
initial ballTextures roughnessTexture dimensions =
    let
        time =
            -- TODO: consider getting the initial time
            Time.millisToPosix 0
    in
    { world = Bodies.world
    , ballTextures = ballTextures
    , roughnessTexture = roughnessTexture
    , time = time
    , dimensions = dimensions
    , zoom = Animator.init 0.9
    , azimuth = Animator.init (Angle.degrees -25)
    , elevation = Animator.init (Angle.degrees 30)
    , focalPoint = Animator.init Point3d.origin
    , orbiting = Nothing
    , state = PlacingBall OutsideOfTable (BehindHeadString (EightBall.rack time EightBall.start))
    }


camera : Model -> Camera3d Meters WorldCoordinates
camera { azimuth, elevation, zoom, focalPoint } =
    let
        distance =
            Animator.move zoom Animator.at
                |> Quantity.interpolateFrom (Length.meters 0.5) (Length.meters 5)
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = pointFromTimeline focalPoint
                , groundPlane = SketchPlane3d.xy
                , azimuth = angleFromTimeline azimuth
                , elevation = angleFromTimeline elevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


ray : Model -> Point2d Pixels ScreenCoordinates -> Axis3d Meters WorldCoordinates
ray model =
    let
        ( width, height ) =
            model.dimensions
    in
    Camera3d.ray
        (camera model)
        (Rectangle2d.with { x1 = pixels 0, y1 = height, x2 = width, y2 = pixels 0 })


view : Model -> Html Msg
view ({ world, ballTextures, roughnessTexture, dimensions } as model) =
    let
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

        entities =
            List.map
                (Bodies.bodyToEntity roughnessTexture ballTextures)
                (World.bodies world)

        entitiesWithUI =
            case model.state of
                PlacingBall (OnTable spawn position) poolWithBallInHand ->
                    let
                        highlightAreaEntity =
                            case poolWithBallInHand of
                                BehindHeadString _ ->
                                    Bodies.areaBehindTheHeadStringEntity

                                Anywhere _ ->
                                    Scene3d.nothing

                        cueBallEntity =
                            Bodies.cueBallEntity (spawn == CanPlace)
                                |> Scene3d.placeIn (Frame3d.atPoint position)
                    in
                    cueBallEntity :: highlightAreaEntity :: entities

                PlacingBall OutsideOfTable (BehindHeadString _) ->
                    Bodies.areaBehindTheHeadStringEntity :: entities

                Playing _ cue _ ->
                    let
                        axis =
                            cueAxis (cueBallPosition model.world) model.azimuth cue

                        isActive =
                            canShoot axis world
                    in
                    Bodies.cueEntity camera3d axis isActive :: entities

                _ ->
                    entities
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "cursor" (currentCursor model.state)
        , Html.Events.preventDefaultOn "wheel"
            (Json.Decode.map
                (\deltaY -> ( MouseWheel deltaY, True ))
                (Json.Decode.field "deltaY" Json.Decode.float)
            )
        ]
        [ Scene3d.custom
            { dimensions = Tuple.mapBoth Quantity.round Quantity.round dimensions
            , antialiasing = Scene3d.noAntialiasing
            , camera = camera3d
            , entities = entitiesWithUI
            , lights = Scene3d.twoLights environmentalLighting sunlight
            , exposure = Scene3d.exposureValue 13
            , whiteBalance = Scene3d.Light.daylight
            , clipDepth = Bodies.clipDepth
            , background = Scene3d.backgroundColor Color.black
            , toneMapping = Scene3d.noToneMapping
            }
        , viewShootingStrength model
        ]


{-| Axis from the hit point on the cue ball along the cue
-}
cueAxis : Point3d Meters WorldCoordinates -> Timeline Angle -> Cue -> Axis3d Meters WorldCoordinates
cueAxis ballPosition cameraAzimuthTimeline { hitRelativeAzimuth, elevation, hitElevation } =
    let
        cameraAzimuth =
            angleFromTimeline cameraAzimuthTimeline

        hitAzimuth =
            cameraAzimuth
                |> Quantity.plus hitRelativeAzimuth

        pointDirection =
            Direction3d.xyZ hitAzimuth hitElevation

        pointOnCueBall =
            Point3d.translateIn pointDirection Bodies.ballRadius ballPosition

        axisDirection =
            Direction3d.xyZ cameraAzimuth elevation
    in
    Axis3d.through pointOnCueBall axisDirection


{-| Check if the cue doesn't overlap with any other objects
-}
canShoot : Axis3d Meters WorldCoordinates -> World Id -> Bool
canShoot axis world =
    let
        direction =
            Axis3d.direction axis

        -- point on the perimeter of the tip of the cue cylinder,
        -- where the cue is placed at the hit point on the ball
        pointOnCueEnd =
            Point3d.translateIn
                (Direction3d.perpendicularTo direction)
                Bodies.cueRadius
                (Axis3d.originPoint axis)

        -- ignore collision with the cue ball
        worldWithoutCueBall =
            World.keepIf (\b -> Body.data b /= CueBall) world
    in
    -- cast 8 rays along the surface of the cue cylinder
    List.all
        (\n ->
            let
                rotatedPoint =
                    pointOnCueEnd
                        |> Point3d.rotateAround axis (Angle.turns (toFloat n / 8))

                cueRay =
                    Axis3d.through rotatedPoint direction
            in
            case World.raycast cueRay worldWithoutCueBall of
                Just { point, body } ->
                    let
                        collisionPoint =
                            point |> Point3d.placeIn (Body.frame body)
                    in
                    -- if the distance is greater than the cue length + offset, then there is no overlap
                    rotatedPoint
                        |> Point3d.distanceFrom collisionPoint
                        |> Quantity.greaterThan (Quantity.plus Bodies.cueOffset Bodies.cueLength)

                Nothing ->
                    True
        )
        (List.range 0 7)


viewShootingStrength : Model -> Html Msg
viewShootingStrength { state, time, dimensions } =
    case state of
        Playing _ { shootPressedAt } _ ->
            case shootPressedAt of
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


currentCursor : State -> String
currentCursor state =
    case state of
        PlacingBall (OnTable _ _) _ ->
            "none"

        Playing (HoveringCueBall _) _ _ ->
            "pointer"

        Playing (ElevatingCue _) _ _ ->
            "ns-resize"

        Simulating _ _ ->
            "wait"

        _ ->
            "default"


currentPlayer : State -> String
currentPlayer state =
    let
        currentPlayer_ =
            case state of
                PlacingBall _ (BehindHeadString pool) ->
                    EightBall.currentPlayer pool

                Playing _ _ pool ->
                    EightBall.currentPlayer pool

                Simulating _ pool ->
                    EightBall.currentPlayer pool

                PlacingBall _ (Anywhere pool) ->
                    EightBall.currentPlayer pool

                GameOver winner _ ->
                    winner
    in
    case currentPlayer_ of
        EightBall.Player1 ->
            "Player 1"

        EightBall.Player2 ->
            "Player 2"


currentTarget : State -> String
currentTarget state =
    let
        currentTarget_ =
            case state of
                PlacingBall _ (BehindHeadString pool) ->
                    EightBall.currentTarget pool

                Playing _ _ pool ->
                    EightBall.currentTarget pool

                Simulating _ pool ->
                    EightBall.currentTarget pool

                PlacingBall _ (Anywhere pool) ->
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
        , Browser.Events.onKeyDown (decodeKey ShootPressed)
        , Browser.Events.onKeyUp (decodeKey ShootReleased)
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
                            EightBall.IllegalBreak newPool ->
                                { newModel
                                    | world = Bodies.world -- Reset the table.
                                    , state = PlacingBall OutsideOfTable (BehindHeadString (EightBall.rack time newPool))
                                    , focalPoint = Animator.go Animator.quickly Point3d.origin newModel.focalPoint
                                }

                            EightBall.PlayersFault newPool ->
                                { newModel
                                    | state = PlacingBall OutsideOfTable (Anywhere newPool)
                                    , world = World.keepIf (\b -> Body.data b /= CueBall) newModel.world
                                    , focalPoint = Animator.go Animator.quickly Point3d.origin newModel.focalPoint
                                }

                            EightBall.NextShot newPool ->
                                { newModel
                                    | state = Playing OutsideOfCueBall initialCue newPool
                                    , focalPoint = Animator.go Animator.quickly (cueBallPosition newModel.world) newModel.focalPoint
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
                    clamp 0 1 (Animator.move model.zoom Animator.at - deltaY * 0.002)
            in
            { model | zoom = Animator.go Animator.immediately newZoom model.zoom }

        MouseDown mousePosition ->
            case model.state of
                PlacingBall (OnTable CanPlace position) poolWithBallInHand ->
                    let
                        newPool =
                            case poolWithBallInHand of
                                BehindHeadString pool ->
                                    EightBall.placeBallBehindHeadstring model.time pool

                                Anywhere pool ->
                                    EightBall.placeBallInHand model.time pool
                    in
                    { model
                        | state = Playing OutsideOfCueBall initialCue newPool
                        , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                        , focalPoint = Animator.go Animator.quickly position model.focalPoint
                    }

                -- these two cases are for preventing orbiting, because
                -- in this case we render a grayed out cue ball at the cursor
                PlacingBall (OnTable CannotPlace _) _ ->
                    model

                Playing (HoveringCueBall { hitRelativeAzimuth, hitElevation }) cue pool ->
                    let
                        newCue =
                            { cue
                                | hitRelativeAzimuth = hitRelativeAzimuth
                                , hitElevation = hitElevation
                            }
                    in
                    { model | state = Playing (ElevatingCue mousePosition) newCue pool }

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
                        PlacingBall _ pool ->
                            let
                                placingArea =
                                    case pool of
                                        Anywhere _ ->
                                            Bodies.areaBallInHand

                                        BehindHeadString _ ->
                                            Bodies.areaBehindTheHeadString

                                newBallInHand =
                                    placeBallInHand (ray model mousePosition) placingArea model.world
                            in
                            { model | state = PlacingBall newBallInHand pool }

                        Playing (ElevatingCue originalPosition) cue pool ->
                            let
                                newElevation =
                                    cueElevation originalPosition mousePosition model.zoom cue.elevation

                                newCue =
                                    { cue | elevation = newElevation }
                            in
                            { model | state = Playing (ElevatingCue mousePosition) newCue pool }

                        Playing _ cue pool ->
                            let
                                newMouse =
                                    hoverCueBall (ray model mousePosition) model.world model.azimuth
                            in
                            { model | state = Playing newMouse cue pool }

                        _ ->
                            model

        MouseUp ->
            case model.state of
                Playing _ cue pool ->
                    { model
                        | state = Playing OutsideOfCueBall cue pool
                        , orbiting = Nothing
                    }

                _ ->
                    { model | orbiting = Nothing }

        ShootPressed ->
            case model.state of
                Playing mouse cue pool ->
                    let
                        axis =
                            cueAxis (cueBallPosition model.world) model.azimuth cue
                    in
                    -- the message can be sent many times
                    -- we need to check if the button isn't already pressed
                    if canShoot axis model.world && cue.shootPressedAt == Nothing then
                        let
                            -- save the time the buttom was pressed
                            newCue =
                                { cue | shootPressedAt = Just model.time }
                        in
                        { model | state = Playing mouse newCue pool }

                    else
                        model

                _ ->
                    model

        ShootReleased ->
            case model.state of
                Playing mouse cue pool ->
                    let
                        axis =
                            cueAxis (cueBallPosition model.world) model.azimuth cue
                    in
                    case ( canShoot axis model.world, cue.shootPressedAt ) of
                        ( True, Just startTime ) ->
                            { model
                                | state = Simulating [] pool
                                , zoom = Animator.go Animator.verySlowly 1 model.zoom
                                , elevation = Animator.go Animator.verySlowly (Angle.degrees 50) model.elevation
                                , world = shoot axis startTime model.time (EightBall.isBreak pool) model.world
                            }

                        _ ->
                            { model | state = Playing mouse { cue | shootPressedAt = Nothing } pool }

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
                    hitAzimuth
                        |> Quantity.minus azimuth
                        |> Angle.normalize

                hoveringFrontHemisphere =
                    -- Prevent from hoveing the back hemisphere when looking from the top
                    Quantity.lessThan (Angle.degrees 90) (Quantity.abs hitRelativeAzimuth)
            in
            if Body.data body == CueBall && hoveringFrontHemisphere then
                HoveringCueBall
                    { hitRelativeAzimuth = hitRelativeAzimuth
                    , hitElevation = hitElevation
                    }

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


{-| Calculate the new cue elevation using the exising elevation and the mouse y offset.

The precision depends on the zoom level.

-}
cueElevation : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Timeline Float -> Angle -> Angle
cueElevation originalPosition newPosition zoomTimeline elevation =
    let
        radiansInPixels =
            orbitingPrecision zoomTimeline

        deltaElevation =
            Vector2d.from originalPosition newPosition
                |> Vector2d.yComponent
                |> Quantity.at radiansInPixels
    in
    elevation
        |> Quantity.minus deltaElevation
        |> Quantity.clamp (Angle.degrees 0) (Angle.degrees 90)


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


placeBallInHand : Axis3d Meters WorldCoordinates -> Rectangle3d Meters WorldCoordinates -> World Id -> BallInHand
placeBallInHand mouseRay spawnArea world =
    let
        -- raise the interection rectangles to vertically align with the center of the ball
        elevatedWholeTableArea =
            Bodies.areaBallInHand
                |> Rectangle3d.translateIn Direction3d.z Bodies.ballRadius

        elevatedSpawnArea =
            spawnArea
                |> Rectangle3d.translateIn Direction3d.z Bodies.ballRadius
    in
    case Axis3d.intersectionWithRectangle elevatedWholeTableArea mouseRay of
        Just position ->
            case Axis3d.intersectionWithRectangle elevatedSpawnArea mouseRay of
                Just _ ->
                    let
                        canPlace =
                            List.all
                                (\body ->
                                    case Body.data body of
                                        Numbered _ ->
                                            Point3d.distanceFrom position (Body.originPoint body)
                                                |> Quantity.greaterThan (Quantity.twice Bodies.ballRadius)

                                        _ ->
                                            True
                                )
                                (World.bodies world)
                    in
                    if canPlace then
                        OnTable CanPlace position

                    else
                        OnTable CannotPlace position

                Nothing ->
                    OnTable CannotPlace position

        _ ->
            OutsideOfTable


{-| Get the position of the cue ball from the world
-}
cueBallPosition : World Id -> Point3d Meters WorldCoordinates
cueBallPosition world =
    World.bodies world
        |> List.filter (\b -> Body.data b == CueBall)
        |> List.head
        |> Maybe.map Body.originPoint
        |> Maybe.withDefault Point3d.origin


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


{-| Simulate multiple frames and collect the game events
-}
simulateWithEvents : Int -> Posix -> World Id -> List ( Posix, ShotEvent ) -> ( World Id, List ( Posix, ShotEvent ) )
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


{-| Read the point value from the timeline
-}
pointFromTimeline : Timeline (Point3d Meters WorldCoordinates) -> Point3d Meters WorldCoordinates
pointFromTimeline pointTimeline =
    Point3d.fromRecord Length.meters <|
        Animator.xyz pointTimeline
            (Point3d.toMeters
                >> (\p -> { x = Animator.at p.x, y = Animator.at p.y, z = Animator.at p.z })
            )


{-| Read the angle value from the timeline
-}
angleFromTimeline : Timeline Angle -> Angle
angleFromTimeline angleTimeline =
    Angle.radians (Animator.move angleTimeline (Angle.inRadians >> Animator.at))


{-| Make orbiting precision depend on zoom level.
Controls how much radians correspond to the change in mouse offset.
-}
orbitingPrecision : Timeline Float -> Quantity Float (Quantity.Rate Angle.Radians Pixels)
orbitingPrecision zoomTimeline =
    Quantity.rate
        (Angle.radians (0.2 + Animator.move zoomTimeline Animator.at / 0.8))
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
