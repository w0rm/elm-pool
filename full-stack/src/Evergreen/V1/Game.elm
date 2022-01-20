module Evergreen.V1.Game exposing (..)

import Angle
import Color
import Dict
import Duration
import Evergreen.V1.Animator
import Evergreen.V1.Bodies
import Evergreen.V1.EightBall
import Evergreen.V1.Physics.Coordinates
import Evergreen.V1.Physics.World
import Evergreen.V1.Point2d
import Evergreen.V1.Point3d
import Evergreen.V1.Scene3d.Material
import Length
import Pixels
import Quantity
import Time


type ScreenCoordinates
    = ScreenCoordinates


type PlacingBallMouse
    = CanSpawnAt (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates)
    | CannotSpawn (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates)
    | HoveringOuside


type alias PlacingBallState awaitingWhat = 
    { pool : (Evergreen.V1.EightBall.Pool awaitingWhat)
    , mouse : PlacingBallMouse
    }


type PlayingMouse
    = HoveringCueBall
    | SettingCueElevation (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinates)
    | NothingMeaningful


type alias PlayingState = 
    { pool : (Evergreen.V1.EightBall.Pool Evergreen.V1.EightBall.AwaitingNextShot)
    , cueBallPosition : (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates)
    , cueElevation : Angle.Angle
    , hitElevation : Angle.Angle
    , hitRelativeAzimuth : Angle.Angle
    , shootButton : (Maybe Duration.Duration)
    , mouse : PlayingMouse
    }


type alias SimulatingState = 
    { events : (List (Time.Posix, Evergreen.V1.EightBall.ShotEvent))
    , pool : (Evergreen.V1.EightBall.Pool Evergreen.V1.EightBall.AwaitingNextShot)
    }


type State
    = PlacingBehindHeadString (PlacingBallState Evergreen.V1.EightBall.AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingState
    | Simulating SimulatingState
    | PlacingBallInHand (PlacingBallState Evergreen.V1.EightBall.AwaitingBallInHand)
    | GameOver (Evergreen.V1.EightBall.Pool Evergreen.V1.EightBall.AwaitingNewGame) Int


type alias Camera = 
    { zoom : Float
    , azimuth : Angle.Angle
    , elevation : Angle.Angle
    }


type alias Model = 
    { world : (Evergreen.V1.Physics.World.World Evergreen.V1.Bodies.Id)
    , ballTextures : (Dict.Dict Int (Evergreen.V1.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Evergreen.V1.Scene3d.Material.Texture Float)
    , dimensions : ((Quantity.Quantity Float Pixels.Pixels), (Quantity.Quantity Float Pixels.Pixels))
    , orbiting : (Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinates))
    , state : State
    , time : Time.Posix
    , cameraTimeline : (Evergreen.V1.Animator.Timeline Camera)
    , focalPointTimeline : (Evergreen.V1.Animator.Timeline (Evergreen.V1.Point3d.Point3d Length.Meters Evergreen.V1.Physics.Coordinates.WorldCoordinates))
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
    | StartNewGameButtonClicked


type alias CueData = 
    { cueElevation : Angle.Angle
    , hitElevation : Angle.Angle
    , hitRelativeAzimuth : Angle.Angle
    }