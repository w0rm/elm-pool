module Pool exposing
    ( Pool, eightBall
    , Action(..), update, nextAction
    , Ball(..)
    )

{-| Pool game rules. Agnostic to game engine.

Ideas:

  - `Valid` for enforcing safe state
  - Use phantom type to enforce safe state
    -- <https://jfmengels.net/single-out-elements-using-phantom-types/>


# Init

@docs Pool, eightBall


# View

@docs currentPlayer


# Update

@docs Action, update, nextAction

-}

import Time



-- Model


type Pool player
    = Pool (PoolData player)


type alias PoolData player =
    { players : List player -- TODO: Consider whether to compare/enforce unique.
    , events : List (Event player)
    , gameFormat : GameFormat
    }


type Event player
    = Event
        { when : Time.Posix
        , type_ : EventType player
        }


type EventType player
    = EventAction (Action player)
    | EventRuling (Ruling player)


type Ruling player
    = Foul -- FoulType
    | Win player


type
    GameFormat
    -- | Format9Ball
    -- | Format10Ball
    = Format8Ball


type Ball
    = Numbered Int
    | Cue



-- Init


{-| Start a game of 8-ball.
-}
eightBall : player -> player -> Pool player
eightBall player1 player2 =
    Pool
        { players = [ player1, player2 ]
        , events = []
        , gameFormat = Format8Ball
        }



-- View


currentPlayer : Pool player -> Maybe player
currentPlayer (Pool { players }) =
    List.head players


{-| Possibly return invalid events too:

    history :
        Pool player
        ->
            { events : List Event player
            , errors : List Event player
            }

-}
history : Pool player -> List (Event player)
history (Pool { events }) =
    events



-- Update


type Action player
    = Rack (List Ball)
    | PlaceBallInKitchen player


update : List ( Time.Posix, Action player ) -> Pool player -> Pool player
update actions (Pool data) =
    Debug.todo "update with new events"


{-|

1.  Possibly use `Valid` to ensure valid state:

    nextAction : Pool player -> Result Error ( Valid (Pool player), Action player )

    update : List ( Time.Posix, Action player ) -> Valid (Pool player) -> Pool player

2.  Would there ever be multiple potential actions?

-}
nextAction : Pool player -> Maybe (Action player)
nextAction (Pool data) =
    case data.gameFormat of
        Format8Ball ->
            nextAction8Ball data



-- 8 ball


nextAction8Ball : PoolData player -> Maybe (Action player)
nextAction8Ball data =
    case data.events of
        [] ->
            -- First event, let's rack!
            Just <|
                Rack
                    [ Numbered 1
                    , Numbered 2
                    , Numbered 3
                    , Numbered 4
                    , Numbered 5
                    , Numbered 6
                    , Numbered 7
                    , Numbered 8
                    , Numbered 9
                    , Numbered 10
                    , Numbered 11
                    , Numbered 12
                    , Numbered 13
                    , Numbered 14
                    , Numbered 15
                    ]

        _ ->
            Debug.todo "determine next action"



-- [ firstEvent ] ->
--     data.events
--         |> List.sortBy (\(Event { when }) -> Time.posixToMillis when)
--         |> List.foldl nextAction8BallStepper firstEvent


nextAction8BallStepper : Event player -> Maybe (Action player) -> Maybe (Action player)
nextAction8BallStepper playerEvent maybeAction =
    Nothing
