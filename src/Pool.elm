module Pool exposing
    ( Pool, start
    , currentPlayer, currentScore
    , rack, ballPlacedInKitchen, playerShot
    , cueHitBall, cueStruck
    , Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall
    , WhatHappened(..)
    , ShotEvent
    )

{-| Pool game rules. Agnostic to game engine.


# Init

@docs Pool, start


# View

@docs currentPlayer, currentScore


# Update

@docs rack, ballPlacedInKitchen, playerShot


## Events

@docs cueHitBall, cueStruck


## Balls

@docs Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall


## Ruling

@docs WhatHappened

-}

import Time



-- Model


{-| This has details about the state of the game as it pertains to rules.
This is what goes into your `Model`.
-}
type Pool state
    = Pool PoolData


type alias PoolData =
    { events : List EventData
    , player : Player
    , ballsHitConsecutively : Int
    }


type alias EventData =
    { when : Time.Posix
    , event : InternalEvent
    }



-- Player


type Player
    = Player1
    | Player2


playerToInt : Player -> Int
playerToInt player =
    case player of
        Player1 ->
            0

        Player2 ->
            1


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        Player1 ->
            Player2

        Player2 ->
            Player1



-- Ball


type Ball
    = Ball Int


oneBall : Ball
oneBall =
    Ball 1


twoBall : Ball
twoBall =
    Ball 2


threeBall : Ball
threeBall =
    Ball 3


fourBall : Ball
fourBall =
    Ball 4


fiveBall : Ball
fiveBall =
    Ball 5


sixBall : Ball
sixBall =
    Ball 6


sevenBall : Ball
sevenBall =
    Ball 7


eightBall : Ball
eightBall =
    Ball 8


nineBall : Ball
nineBall =
    Ball 9


tenBall : Ball
tenBall =
    Ball 10


elevenBall : Ball
elevenBall =
    Ball 11


twelveBall : Ball
twelveBall =
    Ball 12


thirteenBall : Ball
thirteenBall =
    Ball 13


fourteenBall : Ball
fourteenBall =
    Ball 14


fifteenBall : Ball
fifteenBall =
    Ball 15


{-| Create a ball with a given number. Returns `Nothing` for numbers outside of [1, 15].

    numberedBall 13 == Just thirteenBall

    numberedBall 66 == Nothing

-}
numberedBall : Int -> Maybe Ball
numberedBall number =
    if number >= 1 && number <= 15 then
        Just (Ball number)

    else
        Nothing



-- Init


{-| Start a game.
-}
start : Pool AwaitingRack
start =
    Pool
        { events = []
        , player = Player1
        , ballsHitConsecutively = 0
        }



-- View


{-| Show the current player, if there is one.

Zero-based:

    Player1 == 0

    Player2 == 1

-}
currentPlayer : Pool state -> Int
currentPlayer (Pool ({ player } as poolData)) =
    playerToInt player


{-| Get the current score.
-}
currentScore : Pool state -> { player1 : Int, player2 : Int }
currentScore (Pool ({ ballsHitConsecutively, player } as poolData)) =
    case player of
        Player1 ->
            { player1 = ballsHitConsecutively
            , player2 = 0
            }

        Player2 ->
            { player1 = 0
            , player2 = ballsHitConsecutively
            }



-- Update


type AwaitingRack
    = AwaitingRack


type AwaitingNextShot
    = AwaitingNextShot


type AwaitingBallInHand
    = AwaitingBallInHand


type AwaitingBallInKitchen
    = AwaitingBallInKitchen


type AwaitingNewGame
    = AwaitingNewGame


rack : Time.Posix -> Pool AwaitingRack -> Pool AwaitingBallInKitchen
rack when (Pool data) =
    Pool
        { data
            | events =
                data.events
                    ++ [ { when = when
                         , event = Racked
                         }
                       ]
        }


type InternalEvent
    = Racked
    | BallPlacedInKitchen
      --       -- Player actions
      --       -- | CallShot Ball Pocket
      --       -- | PlaceBallInHand
    | Shot (List ( Time.Posix, ShotEvent ))


type ShotEvent
    = CueStruck
      --       -- | BallOffTable Ball
      --       -- | BallToBall Ball Ball (List Ball)
      --       -- | BallToWall Ball Wall
      --       -- | BallToPocket Ball Pocket
    | CueHitBall Ball


ballPlacedInKitchen : Time.Posix -> Pool AwaitingBallInKitchen -> Pool AwaitingNextShot
ballPlacedInKitchen when (Pool data) =
    Pool
        { data
            | events =
                data.events
                    ++ [ { when = when
                         , event = BallPlacedInKitchen
                         }
                       ]
        }


type Event
    = Event EventData


cueHitBall : Time.Posix -> Ball -> ( Time.Posix, ShotEvent )
cueHitBall when ball =
    ( when
    , CueHitBall ball
    )


cueStruck : Time.Posix -> ( Time.Posix, ShotEvent )
cueStruck when =
    ( when
    , CueStruck
    )


twoBallsCollided : Time.Posix -> ( Time.Posix, ShotEvent )
twoBallsCollided =
    Debug.todo ""


ballTouchedTheWall : Time.Posix -> ( Time.Posix, ShotEvent )
ballTouchedTheWall =
    Debug.todo ""


ballFellInPocket : Time.Posix -> ( Time.Posix, ShotEvent )
ballFellInPocket =
    Debug.todo ""



-- Ruling


type
    WhatHappened
    -- = PlayersFault (Pool AwaitingBallInHand)
    = NextShot (Pool AwaitingNextShot)
      -- | NextTurn (Pool AwaitingNextTurn)
    | GameOver (Pool AwaitingNewGame) { winner : Int }
    | Error String


playerShot : List ( Time.Posix, ShotEvent ) -> Pool AwaitingNextShot -> WhatHappened
playerShot shotEvents (Pool data) =
    case shotEvents of
        [] ->
            NextShot <| Pool data

        ( firstShotTime, firstShotEvent ) :: otherShotEvents ->
            let
                allEventDataSorted =
                    data.events
                        ++ [ { event = Shot shotEvents -- TODO: Sort these shotEvents.
                             , when = firstShotTime
                             }
                           ]
                        |> List.sortWith eventTimeComparison

                newPoolData =
                    { data
                        | events = allEventDataSorted
                    }
            in
            checkShot shotEvents newPoolData


{-| TODO: May need to check for equal times and put things like Racked before BallPlacedInKitchen.
-}
eventTimeComparison : EventData -> EventData -> Order
eventTimeComparison eventData1 eventData2 =
    compare
        (Time.toMillis Time.utc eventData1.when)
        (Time.toMillis Time.utc eventData2.when)


checkShot : List ( Time.Posix, ShotEvent ) -> PoolData -> WhatHappened
checkShot shotEvents poolData =
    case shotEvents of
        [] ->
            if poolData.ballsHitConsecutively >= 2 then
                GameOver
                    (Pool poolData)
                    { winner = playerToInt poolData.player
                    }

            else
                NextShot <|
                    Pool poolData

        ( _, CueStruck ) :: otherShots ->
            let
                newPoolData =
                    { poolData
                        | player = switchPlayer poolData.player
                        , ballsHitConsecutively = 0
                    }
            in
            -- TODO: Check CueStruck in otherShots (should only exist once per shot).
            checkShot otherShots newPoolData

        ( _, CueHitBall ball ) :: otherShots ->
            -- TODO (8ball): Check if ball is player's object ball.
            let
                newPoolData =
                    { poolData
                        | ballsHitConsecutively = poolData.ballsHitConsecutively + 1
                    }
            in
            checkShot otherShots newPoolData
