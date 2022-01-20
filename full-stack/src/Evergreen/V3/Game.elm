module Evergreen.V3.Game exposing (..)

import Angle
import Color
import Dict
import Duration
import Evergreen.V3.Animator
import Evergreen.V3.Bodies
import Evergreen.V3.EightBall
import Evergreen.V3.Physics.Coordinates
import Evergreen.V3.Physics.World
import Evergreen.V3.Point2d
import Evergreen.V3.Point3d
import Evergreen.V3.Scene3d.Material
import Length
import Pixels
import Quantity
import Time


type ScreenCoordinates
    = ScreenCoordinates


type PlacingBallMouse
    = CanSpawnAt (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates)
    | CannotSpawn (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates)
    | HoveringOuside


type alias PlacingBallState awaitingWhat = 
    { pool : (Evergreen.V3.EightBall.Pool awaitingWhat)
    , mouse : PlacingBallMouse
    }


type PlayingMouse
    = HoveringCueBall
    | SettingCueElevation (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinates)
    | NothingMeaningful


type alias PlayingState = 
    { pool : (Evergreen.V3.EightBall.Pool Evergreen.V3.EightBall.AwaitingPlayerShot)
    , cueBallPosition : (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates)
    , cueElevation : Angle.Angle
    , hitElevation : Angle.Angle
    , hitRelativeAzimuth : Angle.Angle
    , shootButton : (Maybe Duration.Duration)
    , mouse : PlayingMouse
    }


type alias SimulatingState = 
    { events : (List (Time.Posix, Evergreen.V3.EightBall.ShotEvent))
    , pool : (Evergreen.V3.EightBall.Pool Evergreen.V3.EightBall.AwaitingPlayerShot)
    }


type State
    = PlacingBehindHeadString (PlacingBallState Evergreen.V3.EightBall.AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingState
    | Simulating SimulatingState
    | PlacingBallInHand (PlacingBallState Evergreen.V3.EightBall.AwaitingPlaceBallInHand)
    | GameOver (Evergreen.V3.EightBall.Pool Evergreen.V3.EightBall.AwaitingStart) Int


type alias Camera = 
    { zoom : Float
    , azimuth : Angle.Angle
    , elevation : Angle.Angle
    }


type alias Model = 
    { world : (Evergreen.V3.Physics.World.World Evergreen.V3.Bodies.Id)
    , ballTextures : (Dict.Dict Int (Evergreen.V3.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Evergreen.V3.Scene3d.Material.Texture Float)
    , dimensions : ((Quantity.Quantity Float Pixels.Pixels), (Quantity.Quantity Float Pixels.Pixels))
    , orbiting : (Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinates))
    , state : State
    , time : Time.Posix
    , cameraTimeline : (Evergreen.V3.Animator.Timeline Camera)
    , focalPointTimeline : (Evergreen.V3.Animator.Timeline (Evergreen.V3.Point3d.Point3d Length.Meters Evergreen.V3.Physics.Coordinates.WorldCoordinates))
    }


type alias CueData = 
    { cueElevation : Angle.Angle
    , hitElevation : Angle.Angle
    , hitRelativeAzimuth : Angle.Angle
    }


type Msg
    = Tick Time.Posix
    | Resize Int Int
    | MouseWheel Float
    | MouseDown 
    { x : Float
    , y : Float
    }
    | MouseUp
    | MouseMove 
    { x : Float
    , y : Float
    }
    | ShootButtonDown
    | ShootButtonUp