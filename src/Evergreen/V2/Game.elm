module Evergreen.V2.Game exposing (..)

import Angle
import Color
import Dict
import Duration
import Evergreen.V2.Animator
import Evergreen.V2.Bodies
import Evergreen.V2.EightBall
import Evergreen.V2.Physics.Coordinates
import Evergreen.V2.Physics.World
import Evergreen.V2.Point2d
import Evergreen.V2.Point3d
import Evergreen.V2.Scene3d.Material
import Length
import Pixels
import Quantity
import Time


type ScreenCoordinates
    = ScreenCoordinates


type PlacingBallMouse
    = CanSpawnAt (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates)
    | CannotSpawn (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates)
    | HoveringOuside


type alias PlacingBallState awaitingWhat = 
    { pool : (Evergreen.V2.EightBall.Pool awaitingWhat)
    , mouse : PlacingBallMouse
    }


type PlayingMouse
    = HoveringCueBall
    | SettingCueElevation (Evergreen.V2.Point2d.Point2d Pixels.Pixels ScreenCoordinates)
    | NothingMeaningful


type alias PlayingState = 
    { pool : (Evergreen.V2.EightBall.Pool Evergreen.V2.EightBall.AwaitingNextShot)
    , cueBallPosition : (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates)
    , cueElevation : Angle.Angle
    , hitElevation : Angle.Angle
    , hitRelativeAzimuth : Angle.Angle
    , shootButton : (Maybe Duration.Duration)
    , mouse : PlayingMouse
    }


type alias SimulatingState = 
    { events : (List (Time.Posix, Evergreen.V2.EightBall.ShotEvent))
    , pool : (Evergreen.V2.EightBall.Pool Evergreen.V2.EightBall.AwaitingNextShot)
    }


type State
    = PlacingBehindHeadString (PlacingBallState Evergreen.V2.EightBall.AwaitingPlaceBallBehindHeadstring)
    | Playing PlayingState
    | Simulating SimulatingState
    | PlacingBallInHand (PlacingBallState Evergreen.V2.EightBall.AwaitingBallInHand)
    | GameOver (Evergreen.V2.EightBall.Pool Evergreen.V2.EightBall.AwaitingNewGame) Int


type alias Camera = 
    { zoom : Float
    , azimuth : Angle.Angle
    , elevation : Angle.Angle
    }


type alias Model = 
    { world : (Evergreen.V2.Physics.World.World Evergreen.V2.Bodies.Id)
    , ballTextures : (Dict.Dict Int (Evergreen.V2.Scene3d.Material.Texture Color.Color))
    , roughnessTexture : (Evergreen.V2.Scene3d.Material.Texture Float)
    , dimensions : ((Quantity.Quantity Float Pixels.Pixels), (Quantity.Quantity Float Pixels.Pixels))
    , orbiting : (Maybe (Evergreen.V2.Point2d.Point2d Pixels.Pixels ScreenCoordinates))
    , state : State
    , time : Time.Posix
    , cameraTimeline : (Evergreen.V2.Animator.Timeline Camera)
    , focalPointTimeline : (Evergreen.V2.Animator.Timeline (Evergreen.V2.Point3d.Point3d Length.Meters Evergreen.V2.Physics.Coordinates.WorldCoordinates))
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
    | StartNewGameButtonClicked