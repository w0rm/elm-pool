module Evergreen.V3.EightBall exposing (..)

import Time


type BallGroup
    = SolidGroup
    | StripeGroup
    | EightGroup


type Ball
    = Ball Int BallGroup


type AwaitingPlaceBallBehindHeadstring
    = AwaitingPlaceBallBehindHeadstring


type ShotEvent
    = BallToPocket Ball
    | BallToWall Ball
    | CueHitBall Ball
    | CueHitWall
    | Scratch


type InternalEvent
    = Racked
    | BallPlacedBehindHeadString
    | BallPlacedInHand
    | Shot (List (Time.Posix, ShotEvent))
    | GameOver_


type alias EventData = 
    { when : Time.Posix
    , event : InternalEvent
    }


type Player
    = Player1
    | Player2


type TargetBalls
    = Open
    | Grouped 
    { solids : Player
    }


type alias PoolData = 
    { events : (List EventData)
    , player : Player
    , pocketed : (List (Ball, Player))
    , target : TargetBalls
    }


type Pool state
    = Pool PoolData


type AwaitingPlayerShot
    = AwaitingPlayerShot


type AwaitingPlaceBallInHand
    = AwaitingPlaceBallInHand


type AwaitingStart
    = AwaitingStart