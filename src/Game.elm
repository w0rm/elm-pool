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
import Axis3d exposing (Axis3d)
import Bodies exposing (Id(..))
import Browser.Events
import Camera exposing (Camera, ScreenCoordinates)
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
import Physics.Body as Body exposing (Body)
import Physics.Contact as Contact
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Rectangle2d exposing (Rectangle2d)
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



-- MODEL


type alias Model =
    { time : Posix
    , world : World Id
    , state : State
    , camera : Camera
    , orbiting : Maybe (Point2d Pixels ScreenCoordinates)
    }


type State
    = PlacingBall BallInHand PoolWithBallInHand
    | Shooting AimingCue Shot (Pool EightBall.AwaitingPlayerShot)
    | Simulating (List ( Posix, ShotEvent )) (Pool EightBall.AwaitingPlayerShot)
    | GameOver Player (Pool EightBall.AwaitingStart)


type BallInHand
    = OnTable CanPlace (Point3d Meters WorldCoordinates)
    | OutsideOfTable


type CanPlace
    = CanPlace
    | CannotPlace


type PoolWithBallInHand
    = BehindHeadString (Pool EightBall.AwaitingPlaceBallBehindHeadstring)
    | Anywhere (Pool EightBall.AwaitingPlaceBallInHand)


type AimingCue
    = TargetingCueBall (Maybe HitTarget)
    | ElevatingCue (Point2d Pixels ScreenCoordinates)


type alias Shot =
    { cueElevation : Angle
    , shootPressedAt : Maybe Posix
    , hitTarget : HitTarget
    }


{-| Polar coordinates of the hit point on the surface of the cue ball
-}
type alias HitTarget =
    { relativeAzimuth : Angle -- relative to the camera azimuth
    , elevation : Angle
    }


initialShot : Shot
initialShot =
    { cueElevation = Angle.degrees 5
    , shootPressedAt = Nothing
    , hitTarget = HitTarget (Angle.degrees 0) (Angle.degrees 0) -- aim at the center by default
    }


initialState : Posix -> Pool EightBall.AwaitingRack -> State
initialState time pool =
    PlacingBall OutsideOfTable (BehindHeadString (EightBall.rack time pool))


initial : Model
initial =
    let
        time =
            -- TODO: consider getting the initial time
            Time.millisToPosix 0
    in
    { world = Bodies.world
    , time = time
    , camera = Camera.initial
    , state = initialState time EightBall.start
    , orbiting = Nothing
    }



-- UPDATE


type Msg
    = Tick Posix
    | MouseWheel Float
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | ShootPressed
    | ShootReleased


update : Rectangle2d Pixels ScreenCoordinates -> Msg -> Model -> Model
update window msg oldModel =
    let
        model =
            preUpdate msg oldModel
    in
    case ( model.state, msg ) of
        -- Start by moving the ball above the table
        ( PlacingBall _ pool, MouseMove mousePosition ) ->
            let
                placingArea =
                    case pool of
                        Anywhere _ ->
                            Bodies.areaBallInHand

                        BehindHeadString _ ->
                            Bodies.areaBehindTheHeadString

                mouseRay =
                    Camera.ray model.camera window mousePosition

                newBallInHand =
                    placeBallInHand mouseRay placingArea model.world
            in
            { model | state = PlacingBall newBallInHand pool }

        -- If the ball is on the table and doesn't overlap other balls then place it
        ( PlacingBall (OnTable CanPlace position) poolWithBallInHand, MouseDown _ ) ->
            let
                newPool =
                    case poolWithBallInHand of
                        BehindHeadString pool ->
                            EightBall.placeBallBehindHeadstring model.time pool

                        Anywhere pool ->
                            EightBall.placeBallInHand model.time pool
            in
            { model
                | state = Shooting (TargetingCueBall Nothing) initialShot newPool
                , world = World.add (Body.moveTo position Bodies.cueBall) model.world
                , camera = Camera.focusOn position model.camera
            }

        -- If the ball overlaps - do nothing
        ( PlacingBall (OnTable CannotPlace _) _, MouseDown _ ) ->
            -- this case is for preventing orbiting
            model

        -- Moving mouse over the cue ball lets us pick the hit target
        ( Shooting (TargetingCueBall _) shot pool, MouseMove mousePosition ) ->
            let
                mouseRay =
                    Camera.ray model.camera window mousePosition

                hitTarget =
                    targetCueBall mouseRay model.world (Camera.azimuth model.camera)
            in
            { model | state = Shooting (TargetingCueBall hitTarget) shot pool }

        -- Mouse down on the hit target applies it to the next shot to be made
        ( Shooting (TargetingCueBall (Just hitTarget)) shot pool, MouseDown mousePosition ) ->
            let
                newShot =
                    { shot | hitTarget = hitTarget }
            in
            { model | state = Shooting (ElevatingCue mousePosition) newShot pool }

        -- Change the cue elevation by moving the mouse with the button pressed
        ( Shooting (ElevatingCue originalPosition) shot pool, MouseMove mousePosition ) ->
            let
                newElevation =
                    elevateCue originalPosition mousePosition model.camera shot.cueElevation

                newShot =
                    { shot | cueElevation = newElevation }
            in
            { model | state = Shooting (ElevatingCue mousePosition) newShot pool }

        -- Releasing the mouse button stops elevating the cue
        ( Shooting (ElevatingCue _) shot pool, MouseUp ) ->
            { model | state = Shooting (TargetingCueBall Nothing) shot pool }

        -- Holding the shoot button down allows to select the force
        ( Shooting aimingCue shot pool, ShootPressed ) ->
            let
                axis =
                    cueAxis (cueBallPosition model.world) (Camera.azimuth model.camera) shot
            in
            -- the message can be sent many times
            -- we need to check if the button isn't already pressed
            if canShoot axis model.world && shot.shootPressedAt == Nothing then
                let
                    -- save the time the buttom was pressed
                    newShot =
                        { shot | shootPressedAt = Just model.time }
                in
                { model | state = Shooting aimingCue newShot pool }

            else
                model

        -- Releasing the button shoots the ball!
        ( Shooting aimingCue shot pool, ShootReleased ) ->
            let
                axis =
                    cueAxis (cueBallPosition model.world) (Camera.azimuth model.camera) shot

                startTime =
                    Maybe.withDefault model.time shot.shootPressedAt
            in
            if canShoot axis model.world then
                { model
                    | state = Simulating [] pool
                    , camera = Camera.zoomOut model.camera
                    , world = shoot axis startTime model.time (EightBall.isBreak pool) model.world
                }

            else
                { model | state = Shooting aimingCue { shot | shootPressedAt = Nothing } pool }

        -- Simulate the physics!
        ( Simulating events pool, Tick time ) ->
            case simulate time model.world events pool of
                -- Continue simulating on the next tick
                Continue ( newWorld, newEvents ) ->
                    { model
                        | world = newWorld
                        , state = Simulating newEvents pool
                    }

                -- Stop the simulation, decide what to do next!
                Stop (EightBall.IllegalBreak newPool) ->
                    { model
                        | world = Bodies.world -- Reset the table.
                        , state = initialState time newPool
                        , camera = Camera.focusOn Point3d.origin model.camera
                    }

                Stop (EightBall.PlayersFault newPool) ->
                    { model
                        | world = World.keepIf (\b -> Body.data b /= CueBall) model.world
                        , state = PlacingBall OutsideOfTable (Anywhere newPool)
                        , camera = Camera.focusOn Point3d.origin model.camera
                    }

                Stop (EightBall.NextShot newPool) ->
                    let
                        newFocalPoint =
                            cueBallPosition model.world
                    in
                    { model
                        | state = Shooting (TargetingCueBall Nothing) initialShot newPool
                        , camera = Camera.focusOn newFocalPoint model.camera
                    }

                Stop (EightBall.GameOver newPool { winner }) ->
                    { model
                        | state = GameOver winner newPool
                        , camera = Camera.focusOn Point3d.origin model.camera
                    }

        -- this case is here, to let the cases above prevent orbiting
        -- by intercepting the MouseDown event
        ( _, MouseDown mousePosition ) ->
            { model | orbiting = Just mousePosition }

        _ ->
            model


{-| Perform the updates that are always necessary no matter the game state
-}
preUpdate : Msg -> Model -> Model
preUpdate msg model =
    case msg of
        -- advance the time
        Tick time ->
            { model
                | time = time
                , camera = Camera.animate time model.camera
            }

        -- continue orbiting if already started
        MouseMove mousePosition ->
            case model.orbiting of
                Just originalPosition ->
                    -- update the camera orientation
                    -- note that changing the azimuth impacts the cue axis
                    let
                        newCamera =
                            Camera.mouseOrbiting originalPosition mousePosition model.camera
                    in
                    { model | camera = newCamera, orbiting = Just mousePosition }

                Nothing ->
                    model

        -- always stop orbiting on mouse up
        MouseUp ->
            { model | orbiting = Nothing }

        MouseWheel deltaY ->
            { model | camera = Camera.mouseWheelZoom deltaY model.camera }

        _ ->
            model



-- Placing ball in hand


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
                    placeBallInHandHelp (World.bodies world) position

                Nothing ->
                    OnTable CannotPlace position

        _ ->
            OutsideOfTable


{-| Check if overlaps with any of the numbered balls
-}
placeBallInHandHelp : List (Body Id) -> Point3d Meters WorldCoordinates -> BallInHand
placeBallInHandHelp bodies position =
    case bodies of
        body :: remaining ->
            case Body.data body of
                Numbered _ ->
                    if
                        Body.originPoint body
                            |> Point3d.distanceFrom position
                            |> Quantity.lessThan (Quantity.twice Bodies.ballRadius)
                    then
                        OnTable CannotPlace position

                    else
                        placeBallInHandHelp remaining position

                _ ->
                    placeBallInHandHelp remaining position

        [] ->
            OnTable CanPlace position



-- Aiming cue


{-| Pick a point on the cue ball to hit
-}
targetCueBall : Axis3d Meters WorldCoordinates -> World Id -> Angle -> Maybe HitTarget
targetCueBall mouseRay world azimuth =
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

                hitRelativeAzimuth =
                    hitAzimuth
                        |> Quantity.minus azimuth
                        |> Angle.normalize

                hoveringFrontHemisphere =
                    -- Prevent from hoveing the back hemisphere when looking from the top
                    Quantity.lessThan (Angle.degrees 90) (Quantity.abs hitRelativeAzimuth)
            in
            if Body.data body == CueBall && hoveringFrontHemisphere then
                Just
                    { relativeAzimuth = hitRelativeAzimuth
                    , elevation = hitElevation
                    }

            else
                Nothing

        Nothing ->
            Nothing


{-| Calculate the new cue elevation using the exising elevation and the mouse y offset.

The precision depends on the zoom level.

-}
elevateCue : Point2d Pixels ScreenCoordinates -> Point2d Pixels ScreenCoordinates -> Camera -> Angle -> Angle
elevateCue originalPosition newPosition camera elevation =
    let
        radiansInPixels =
            Camera.orbitingPrecision camera

        deltaElevation =
            Vector2d.from originalPosition newPosition
                |> Vector2d.yComponent
                |> Quantity.at radiansInPixels
    in
    elevation
        |> Quantity.minus deltaElevation
        |> Quantity.clamp (Angle.degrees 0) (Angle.degrees 90)



-- Shooting


{-| Get the position of the cue ball from the world
-}
cueBallPosition : World Id -> Point3d Meters WorldCoordinates
cueBallPosition world =
    world
        |> World.keepIf (\b -> Body.data b == CueBall)
        |> World.bodies
        |> List.head
        |> Maybe.map Body.originPoint
        |> Maybe.withDefault Point3d.origin


{-| Axis from the hit point on the cue ball along the cue
-}
cueAxis : Point3d Meters WorldCoordinates -> Angle -> Shot -> Axis3d Meters WorldCoordinates
cueAxis ballPosition cameraAzimuth { hitTarget, cueElevation } =
    let
        hitAzimuth =
            cameraAzimuth
                |> Quantity.plus hitTarget.relativeAzimuth

        pointDirection =
            Direction3d.xyZ hitAzimuth hitTarget.elevation

        pointOnCueBall =
            Point3d.translateIn pointDirection Bodies.ballRadius ballPosition

        axisDirection =
            Direction3d.xyZ cameraAzimuth cueElevation
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



-- Simulation


type SimulatedWorld
    = Continue ( World Id, List ( Posix, ShotEvent ) )
    | Stop EightBall.WhatHappened


simulate : Posix -> World Id -> List ( Posix, ShotEvent ) -> Pool EightBall.AwaitingPlayerShot -> SimulatedWorld
simulate time world events pool =
    let
        ballsStoppedMoving =
            List.all
                (\body ->
                    Body.velocity body
                        |> Vector3d.length
                        |> Quantity.lessThan (Speed.metersPerSecond 0.0005)
                )
                (World.bodies world)
    in
    if ballsStoppedMoving then
        Stop (EightBall.playerShot (List.reverse events) pool)

    else
        Continue (simulateWithEvents 2 time world events)


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



-- VIEW


view : Dict Int (Texture Color) -> Texture Float -> Rectangle2d Pixels ScreenCoordinates -> Model -> Html Msg
view ballTextures roughnessTexture window model =
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
            Camera.camera3d model.camera

        entities =
            List.map
                (Bodies.bodyToEntity roughnessTexture ballTextures)
                (World.bodies model.world)

        dimensions =
            window
                |> Rectangle2d.dimensions
                |> Tuple.mapBoth Quantity.round Quantity.round

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

                Shooting _ cue _ ->
                    let
                        axis =
                            cueAxis (cueBallPosition model.world) (Camera.azimuth model.camera) cue

                        isActive =
                            canShoot axis model.world
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
            { dimensions = dimensions
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
        , viewShootingStrength window model
        ]


viewShootingStrength : Rectangle2d Pixels ScreenCoordinates -> Model -> Html Msg
viewShootingStrength window { state, time } =
    case state of
        Shooting _ { shootPressedAt } _ ->
            case shootPressedAt of
                Nothing ->
                    Html.text ""

                Just startTime ->
                    let
                        progressHeight =
                            shootingStrength startTime time * (barHeight - 4)

                        height =
                            window
                                |> Rectangle2d.dimensions
                                |> Tuple.second
                                |> Pixels.inPixels

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

        Shooting (TargetingCueBall _) _ _ ->
            "pointer"

        Shooting (ElevatingCue _) _ _ ->
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

                Shooting _ _ pool ->
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

                Shooting _ _ pool ->
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (decodeKey ShootPressed)
        , Browser.Events.onKeyUp (decodeKey ShootReleased)
        , Browser.Events.onAnimationFrame Tick
        , Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


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
