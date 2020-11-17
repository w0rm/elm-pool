module Pool exposing
    ( Pool, start
    , currentPlayer
    , rack, ballPlacedInKitchen, playerShot
    , cueHitBall, cueStruck
    , oneBall
    , WhatHappened(..)
    )

{-| Pool game rules. Agnostic to game engine.


# Init

@docs Pool, start


# View

@docs currentPlayer


# Update

@docs rack, ballPlacedInKitchen, playerShot


## Events

@docs cueHitBall, cueStruck


## Balls

@docs oneBall


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



-- type Status =


type alias EventData =
    { when : Time.Posix
    , event : InternalEvent
    }



-- type EventType
--     = EventAction Action
--     | EventRuling Ruling


type Ruling
    = Foul -- FoulType
    | Win


type
    GameFormat
    -- | Format9Ball
    -- | Format10Ball
    = Format8Ball


type Ball
    = OneBall
    | TwoBall
    | ThreeBall


oneBall : Ball
oneBall =
    OneBall


type Cue
    = Cue



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


{-| Show the current player, if there is one.

Zero-based:

    Player1 == 0

    Player2 == 1

-}
currentPlayer : Pool state -> Int
currentPlayer (Pool ({ player } as poolData)) =
    -- Failed attempt to derive all state from events.
    -- case events of
    --     [] ->
    --         0
    --
    --     firstEvent :: [] ->
    --         0
    --
    --     firstEvent :: secondEvent :: otherEvents ->
    --         findCurrentPlayer firstEvent.event secondEvent.event (List.map .event otherEvents) Player1 poolData
    playerToInt player


{-| Recurse through list of events, keeping track of current player. This is the single source of truth approach.

A more efficient way would be to track it separately from events, but also more error-prone.

-}
findCurrentPlayer : InternalEvent -> InternalEvent -> List InternalEvent -> Player -> PoolData -> Int
findCurrentPlayer eventData1 eventData2 remainingEventData player poolData =
    let
        _ =
            Debug.log "player" player
    in
    case remainingEventData of
        [] ->
            case ( eventData1, eventData2 ) of
                ( Racked, BallPlacedInKitchen ) ->
                    playerToInt player

                ( BallPlacedInKitchen, Shot shotEvents ) ->
                    Debug.todo "check shot"

                ( Racked, Racked ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

                ( Racked, _ ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

                ( BallPlacedInKitchen, Racked ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

                ( BallPlacedInKitchen, BallPlacedInKitchen ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

                ( Shot _, Shot shotEvents ) ->
                    Debug.todo "check shot"

                ( Shot _, Racked ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

                ( Shot _, BallPlacedInKitchen ) ->
                    -- Nonsense. How do we prevent/indicate this? Error, Maybe?
                    playerToInt player

        anotherEvent :: otherEvents ->
            findCurrentPlayer BallPlacedInKitchen
                anotherEvent
                otherEvents
                player
                poolData



-- Update


type AwaitingRack
    = AwaitingRack


type AwaitingNextShot
    = AwaitingNextShot



-- type AwaitingCollisions
--     = AwaitingCollisions


type AwaitingBallInHand
    = AwaitingBallInHand


type AwaitingBallInKitchen
    = AwaitingBallInKitchen


type AwaitingNewGame
    = AwaitingNewGame



-- type Action state
--     = Action


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


type WhatHappened
    = PlayersFault (Pool AwaitingBallInHand)
    | NextShot (Pool AwaitingNextShot)
      -- | NextTurn (Pool AwaitingNextTurn)
    | GameOver (Pool AwaitingNewGame) -- { winner : Int, loser: Int }
    | Error String


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



-- Failed attempt to derive all state from events.
-- case allEventDataSorted |> List.map .event of
--     [] ->
--         Error "Should already be racked and ball placed."
--
--     [ singleEvent ] ->
--         Error "Should already be racked and ball placed."
--
--     Racked :: BallPlacedInKitchen :: [] ->
--         NextShot <|
--             Pool newPoolData
--
--     Racked :: BallPlacedInKitchen :: otherEvents ->
--         checkEvents Racked BallPlacedInKitchen otherEvents newPoolData
--
--     firstEvent :: secondEvent :: otherEvents ->
--         Error "Should be racked, then ball in kitchen first"


{-| TODO: May need to check for equal times and put things like Racked before BallPlacedInKitchen and so on.
-}
eventTimeComparison : EventData -> EventData -> Order
eventTimeComparison eventData1 eventData2 =
    compare
        (Time.toMillis Time.utc eventData1.when)
        (Time.toMillis Time.utc eventData2.when)


checkEvents : InternalEvent -> InternalEvent -> List InternalEvent -> PoolData -> WhatHappened
checkEvents eventData1 eventData2 remainingEventData poolData =
    case remainingEventData of
        [] ->
            case ( eventData1, eventData2 ) of
                ( Racked, BallPlacedInKitchen ) ->
                    NextShot <|
                        Pool poolData

                ( BallPlacedInKitchen, Shot shotEvents ) ->
                    checkShot shotEvents poolData

                ( BallPlacedInKitchen, Racked ) ->
                    Error "Nonsense, rack before placing ball in the kitchen!"

                ( BallPlacedInKitchen, BallPlacedInKitchen ) ->
                    -- Once ball is placed in the kitchen, it cannot be moved until a shot is taken.
                    PlayersFault (Pool poolData)

                ( Racked, Shot _ ) ->
                    Error "Ball should be placed in kitchen before shot (this state shouldn't happen)"

                ( Racked, Racked ) ->
                    Error "Only rack once (this state shouldn't happen)"

                ( Shot _, Shot shotEvents ) ->
                    checkShot shotEvents poolData

                ( Shot _, _ ) ->
                    Error "Only shots (or ball in hand) after a shot (this state shouldn't happen)"

        anotherEvent :: otherEvents ->
            checkEvents eventData2 anotherEvent otherEvents poolData


checkShot : List ( Time.Posix, ShotEvent ) -> PoolData -> WhatHappened
checkShot shotEvents poolData =
    case shotEvents of
        [] ->
            if poolData.ballsHitConsecutively >= 2 then
                GameOver <|
                    Pool poolData

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



-- Failed attempt to derive all state from events.
-- checkShotRecursively : ( Time.Posix, ShotEvent ) -> ( Time.Posix, ShotEvent ) -> List ( Time.Posix, ShotEvent ) -> PoolData -> WhatHappened
-- checkShotRecursively ( previousShotTime, previousShot ) (( currentShotTime, currentShotEvent ) as currentShot) otherShotEvents poolData =
--     case otherShotEvents of
--         [] ->
--             case ( previousShot, currentShotEvent ) of
--                 ( CueStruck, CueHitBall _ ) ->
--                     NextShot <|
--                         Pool poolData
--
--                 ( CueHitBall _, CueHitBall _ ) ->
--                     GameOver <|
--                         Pool poolData
--
--         ( CueHitBall _, CueStruck, _ ) ->
--             Error "Cue should only be struck at beginning of shot"
--
--         ( CueStruck, CueStruck, _ ) ->
--             Error "Cue should only be struck at beginning of shot"
--
--         nextShot :: moreShotEvents ->
--             checkShotRecursively currentShot nextShot moreShotEvents poolData


stopTheGame : Pool { a | gameStoppable : Bool } -> Pool AwaitingRack
stopTheGame =
    Debug.todo ""


type Rack
    = Rack_ (List Ball)
