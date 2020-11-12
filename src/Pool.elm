module Pool exposing
    ( Pool, start
    , rack, ballPlacedInKitchen, playerShot
    , cueHitBall
    , oneBall
    , WhatHappened(..)
    -- , Action(..)
    -- , update, nextAction
    -- , currentPlayer, balls
    -- , eightBall
    )

{-| Pool game rules. Agnostic to game engine.

Ideas:

  - `Valid` for enforcing safe state

  - Use phantom type to enforce safe state
      - <https://jfmengels.net/single-out-elements-using-phantom-types/>

      - Add phantom type to `Pool`:

            type Status
                = AwaitingNextShot
                | AwaitingRack
                | AwaitingPlaceBallInHand


# Init

@docs Pool, start


# View


# Update

@docs rack, ballPlacedInKitchen, playerShot


## Events

@docs cueHitBall


## Balls

@docs oneBall


## Ruling

@docs WhatHappened

@docs Action, update, nextAction, rack

-}

import Time


playerTookTurn : WhatHappened
playerTookTurn =
    start
        |> rack (Time.millisToPosix 0)
        |> ballPlacedInKitchen (Time.millisToPosix 0)
        |> playerShot
            [ twoBallsCollided (Time.millisToPosix 1)
            ]



-- test :  Pool state


test whatHappened =
    case playerTookTurn of
        PlayersFault pool ->
            Debug.todo " players fault"

        NextShot _ ->
            Debug.todo "handle NextTurn _"

        Error _ ->
            Debug.todo "handle Error"

        GameOver _ ->
            Debug.todo "handle GameOver _"



-- Model


{-| This has details about the state of the game as it pertains to rules.
This is what goes into your `Model`.
-}
type Pool state
    = Pool PoolData


type alias PoolData =
    { events : List EventData
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
        }



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


cueHitBall : Time.Posix -> Ball -> Event
cueHitBall when ball =
    Event
        { when = when
        , event = CueHitBall ball
        }


twoBallsCollided : Time.Posix -> Event
twoBallsCollided =
    Debug.todo ""


ballTouchedTheWall : Time.Posix -> Event
ballTouchedTheWall =
    Debug.todo ""


ballFellInPocket : Time.Posix -> Event
ballFellInPocket =
    Debug.todo ""


playerShot : List Event -> Pool AwaitingNextShot -> WhatHappened
playerShot events (Pool data) =
    let
        allEventDataSorted =
            data.events
                ++ List.map (\(Event eventData) -> eventData) events
                |> List.sortWith eventTimeComparison

        newPoolData =
            { data
                | events = allEventDataSorted
            }
    in
    case allEventDataSorted |> List.map .event of
        [] ->
            Error "Should already be racked and ball placed."

        [ singleEvent ] ->
            Error "Should already be racked and ball placed."

        Racked :: BallPlacedInKitchen :: [] ->
            NextShot <|
                Pool newPoolData

        Racked :: BallPlacedInKitchen :: otherEvents ->
            checkEvents Racked BallPlacedInKitchen otherEvents newPoolData

        firstEvent :: secondEvent :: otherEvents ->
            Error "Should be racked, then ball in kitchen first"


{-| May need to check for equal times and put things like Racked before BallPlacedInKitchen and so on.
-}
eventTimeComparison : EventData -> EventData -> Order
eventTimeComparison eventData1 eventData2 =
    compare
        (Time.toMillis Time.utc eventData1.when)
        (Time.toMillis Time.utc eventData2.when)


checkEvents : InternalEvent -> InternalEvent -> List InternalEvent -> PoolData -> WhatHappened
checkEvents eventData1 eventData2 remainingEventData poolData =
    case ( eventData1, eventData2, remainingEventData ) of
        ( Racked, BallPlacedInKitchen, [] ) ->
            NextShot <|
                Pool poolData

        ( Racked, BallPlacedInKitchen, anotherEvent :: otherEvents ) ->
            checkEvents eventData2 anotherEvent otherEvents poolData

        ( BallPlacedInKitchen, CueHitBall ball, _ ) ->
            GameOver (Pool poolData)

        ( BallPlacedInKitchen, Racked, _ ) ->
            Error "Nonsense, rack before placing ball in the kitchen!"

        ( BallPlacedInKitchen, BallPlacedInKitchen, _ ) ->
            -- Once ball is placed in the kitchen, it cannot be moved until a shot is taken.
            PlayersFault (Pool poolData)

        ( CueHitBall _, _, _ ) ->
            Debug.todo "handle ( CueHitBall _, _, _ )"

        ( Racked, _, _ ) ->
            Debug.todo "handle ( Racked, Racked, _ )"


stopTheGame : Pool { a | gameStoppable : Bool } -> Pool AwaitingRack
stopTheGame =
    Debug.todo ""


{-| These are the available actions a game engine can use to update a pool game.
-}



-- type Action
--     = -- Table actions
--       -- | BallOffTable Ball
--       -- | BallToBall Ball Ball (List Ball)
--       -- | BallToWall Ball Wall
--       -- | BallToPocket Ball Pocket
--       Rack Rack
--       -- Player actions
--       -- | CallShot
--       -- | PlaceBallInHand
--       -- | Shoot (List Ball)  -- Is this even necessary with `BallToBall`? Maybe in a `Status` type.
--     | PlaceBallInKitchen


type Rack
    = Rack_ (List Ball)



-- {-| Where new events can be sent to update based on the game rules.
--
--     game =
--         Pool.eightBall Player1 Player2
--
--     rackedGame =
--         game
--             |> Pool.update
--                 [ ( Time.millisToPosix 0
--                   , Pool.rack game
--                   )
--                 ]
--
-- -}
-- update : List ( Time.Posix, Action ) -> Pool -> Pool
-- update actions (Pool data) =
--     Debug.todo "update with new events"
-- {-|
--
-- 1.  Possibly use `Valid` to ensure valid state:
--
--     nextAction : Pool -> Result Error ( Valid (Pool ), Valid (Action ) )
--
--     update : List ( Time.Posix, Valid (Action ) ) -> Valid (Pool ) -> Pool
--
-- 2.  Would there ever be multiple potential actions?
--
-- -}
-- nextAction : Pool -> Maybe Action
-- nextAction (Pool data) =
--     case data.gameFormat of
--         Format8Ball ->
--             nextAction8Ball data
-- 8 ball
-- nextAction8Ball : PoolData -> Maybe Action
-- nextAction8Ball data =
--     case data.events of
--         [] ->
--             -- First event, let's rack!
--             Just <|
--                 rack8Ball
--
--         _ ->
--             Debug.todo "determine next action"
-- [ firstEvent ] ->
--     data.events
--         |> List.sortBy (\(Event { when }) -> Time.posixToMillis when)
--         |> List.foldl nextAction8BallStepper firstEvent
-- nextAction8BallStepper : Event -> Maybe Action -> Maybe Action
-- nextAction8BallStepper playerEvent maybeAction =
--     Nothing
-- View
-- {-| Show the current player, if there is one.
-- -}
-- currentPlayer : Pool -> Maybe
-- currentPlayer (Pool { players }) =
--     List.head players
