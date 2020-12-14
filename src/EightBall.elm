module EightBall exposing
    ( Pool, AwaitingRack, AwaitingNextShot, AwaitingBallInHand, AwaitingPlaceBallBehindHeadstring, AwaitingNewGame, start
    , CurrentTarget(..)
    , currentPlayer, currentScore, currentTarget
    , rack, ballPlacedBehindHeadString, playerShot
    , ShotEvent
    , cueHitBall, ballFellInPocket, scratch
    , Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall
    , WhatHappened(..)
    )

{-| Pool game rules. Agnostic to game engine.

8-Ball rules follow the WPA [8-Ball rules](https://wpapool.com/rules-of-play/#eight-ball) with the following exceptions:

  - Shots do not have to be called
  - Determining target group requires to sink only balls in one group.
    Examples:
      - Shooter pockets two solids while table is open (and doesn't scratch), then that player's target will become solids.
      - Shooter pockets a stripe while table is open (and doesn't scratch), then that player's target will become stripes.
      - Shooter pockets a stripe and two solids while table is open (and doesn't scratch), then the table is still open.


# Init

@docs Pool, AwaitingRack, AwaitingNextShot, AwaitingBallInHand, AwaitingPlaceBallBehindHeadstring, AwaitingNewGame, start


# View

@docs CurrentTarget

@docs currentPlayer, currentScore, currentTarget


# Update

@docs rack, ballPlacedBehindHeadString, playerShot


## Events

@docs ShotEvent
@docs cueHitBall, ballFellInPocket, scratch


## Balls

@docs Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall


## Ruling

@docs WhatHappened

-}

import Set
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
    , pocketed : List ( Ball, Player ) -- Ideally this would be a Dictionary, but it would require a bit more work to generate a comparable for each ball.
    , target : TargetBalls
    }


type alias EventData =
    { when : Time.Posix
    , event : InternalEvent
    }


type TargetBalls
    = Open -- Any ball except the eight ball may be struck.
    | Grouped { solids : Player }


pocketedIn : BallGroup -> List ( Ball, Player ) -> Int
pocketedIn group pocketedBalls =
    pocketedBalls
        |> List.filter
            (\( Ball number pocketedBallGroup, player ) ->
                pocketedBallGroup == group
            )
        |> List.length



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
    = Ball Int BallGroup


oneBall : Ball
oneBall =
    Ball 1 SolidGroup


twoBall : Ball
twoBall =
    Ball 2 SolidGroup


threeBall : Ball
threeBall =
    Ball 3 SolidGroup


fourBall : Ball
fourBall =
    Ball 4 SolidGroup


fiveBall : Ball
fiveBall =
    Ball 5 SolidGroup


sixBall : Ball
sixBall =
    Ball 6 SolidGroup


sevenBall : Ball
sevenBall =
    Ball 7 SolidGroup


eightBall : Ball
eightBall =
    Ball 8 EightGroup


nineBall : Ball
nineBall =
    Ball 9 StripeGroup


tenBall : Ball
tenBall =
    Ball 10 StripeGroup


elevenBall : Ball
elevenBall =
    Ball 11 StripeGroup


twelveBall : Ball
twelveBall =
    Ball 12 StripeGroup


thirteenBall : Ball
thirteenBall =
    Ball 13 StripeGroup


fourteenBall : Ball
fourteenBall =
    Ball 14 StripeGroup


fifteenBall : Ball
fifteenBall =
    Ball 15 StripeGroup


{-| Create a ball with a given number. Returns `Nothing` for numbers outside of [1, 15].

    numberedBall 13 == Just thirteenBall

    numberedBall 66 == Nothing

-}
numberedBall : Int -> Maybe Ball
numberedBall number =
    if number <= 0 then
        Nothing

    else if number <= 7 then
        Just (Ball number SolidGroup)

    else if number == 8 then
        Just (Ball 8 EightGroup)

    else if number <= 15 then
        Just (Ball number StripeGroup)

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
        , pocketed = []
        , target = Open
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
currentScore (Pool ({ player, pocketed, target } as poolData)) =
    case target of
        Open ->
            { player1 = 0
            , player2 = 0
            }

        Grouped { solids } ->
            case solids of
                Player1 ->
                    { player1 = pocketedIn SolidGroup pocketed
                    , player2 = pocketedIn StripeGroup pocketed
                    }

                Player2 ->
                    { player1 = pocketedIn StripeGroup pocketed
                    , player2 = pocketedIn SolidGroup pocketed
                    }


type CurrentTarget
    = OpenTable
    | Solids
    | Stripes
    | EightBall


currentTarget : Pool state -> CurrentTarget
currentTarget (Pool poolData) =
    case poolData.target of
        Open ->
            OpenTable

        Grouped { solids } ->
            if solids == poolData.player then
                Solids

            else
                Stripes



-- Update


type AwaitingRack
    = AwaitingRack


type AwaitingNextShot
    = AwaitingNextShot


type AwaitingBallInHand
    = AwaitingBallInHand


{-| This is the area where the player can place the cue ball before a break.

From WPA [rules 8.1 Parts of the Table](https://wpapool.com/rules-of-play/#Definitions):

> Behind the head string is the area between the head rail and the head string, not including the
> head string.

Also known as "in the kitchen".

-}
type AwaitingPlaceBallBehindHeadstring
    = AwaitingPlaceBallBehindHeadstring


type AwaitingNewGame
    = AwaitingNewGame


rack : Time.Posix -> Pool AwaitingRack -> Pool AwaitingPlaceBallBehindHeadstring
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
    | BallPlacedBehindHeadString
      --       -- Player actions
      --       -- | CallShot Ball Pocket
      --       -- | PlaceBallInHand
    | Shot (List ( Time.Posix, ShotEvent ))


type ShotEvent
    = -- CueStruck -- Without a CueStruck event, it's not possible to "replay" events.
      --       -- | BallOffTable Ball
      --       -- | BallToBall Ball Ball (List Ball)
      --       -- | BallToWall Ball Wall
      BallToPocket Ball --Pocket
    | CueHitBall Ball
    | Scratch


ballPlacedBehindHeadString : Time.Posix -> Pool AwaitingPlaceBallBehindHeadstring -> Pool AwaitingNextShot
ballPlacedBehindHeadString when (Pool data) =
    Pool
        { data
            | events =
                data.events
                    ++ [ { when = when
                         , event = BallPlacedBehindHeadString
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


twoBallsCollided : Time.Posix -> ( Time.Posix, ShotEvent )
twoBallsCollided =
    Debug.todo ""


ballTouchedTheWall : Time.Posix -> ( Time.Posix, ShotEvent )
ballTouchedTheWall =
    Debug.todo ""


ballFellInPocket : Time.Posix -> Ball -> ( Time.Posix, ShotEvent )
ballFellInPocket when ball =
    ( when
    , BallToPocket ball
    )


{-| When the cue ball is pocketed.

[WPA Rules 8.6](https://wpapool.com/rules-of-play/#86Scratch)

-}
scratch : Time.Posix -> ( Time.Posix, ShotEvent )
scratch when =
    ( when
    , Scratch
    )



-- Ruling


type WhatHappened
    = PlayersFault (Pool AwaitingBallInHand)
    | NextShot (Pool AwaitingNextShot)
      -- | NextTurn (Pool AwaitingNextTurn)
    | GameOver (Pool AwaitingNewGame) { winner : Int }
    | Error String


{-| Send a series of shot events.

Note: if no balls are hit by the cue ball, send an empty list.

    playerShot [] pool -- Cue struck, but no other balls hit.

-}
playerShot : List ( Time.Posix, ShotEvent ) -> Pool AwaitingNextShot -> WhatHappened
playerShot shotEvents (Pool data) =
    case shotEvents of
        [] ->
            -- Assume the cue is struck, but no other balls are hit.
            NextShot <|
                Pool
                    { data
                        | player = switchPlayer data.player
                        , events =
                            data.events
                                ++ [ { event = Shot []
                                     , when = lastEventTime data.events
                                     }
                                   ]
                                |> List.sortWith eventTimeComparison
                    }

        ( firstShotTime, firstShotEvent ) :: otherShotEvents ->
            let
                allEventDataSorted =
                    data.events
                        ++ [ { event = Shot shotEvents -- TODO: Sort these shotEvents.
                             , when = firstShotTime
                             }
                           ]
                        |> List.sortWith eventTimeComparison

                ballPocketedEvents =
                    groupPocketedEvents shotEvents

                newPoolData =
                    { data
                        | events = allEventDataSorted
                        , target =
                            checkNextTarget
                                ballPocketedEvents
                                data
                        , pocketed =
                            updatePocketed
                                ballPocketedEvents
                                data
                    }
            in
            checkShot shotEvents
                ballPocketedEvents
                newPoolData


{-| TODO: May need to check for equal times and put things like Racked before BallPlacedBehindHeadString.
-}
eventTimeComparison : EventData -> EventData -> Order
eventTimeComparison eventData1 eventData2 =
    compare
        (Time.toMillis Time.utc eventData1.when)
        (Time.toMillis Time.utc eventData2.when)


{-| TODO: May need to check for equal times and put things like CueHitBall before BallToPocket.
-}
shotEventTimeComparison : ( Time.Posix, ShotEvent ) -> ( Time.Posix, ShotEvent ) -> Order
shotEventTimeComparison ( time1, _ ) ( time2, _ ) =
    compare
        (Time.toMillis Time.utc time1)
        (Time.toMillis Time.utc time2)


checkNextTarget : BallPocketedEvents -> PoolData -> TargetBalls
checkNextTarget { allPocketedBalls, solidsPocketed, stripesPocketed } poolData =
    case poolData.target of
        Open ->
            if
                List.length allPocketedBalls > 0
                -- TODO: and player did not foul.
            then
                if List.length solidsPocketed == List.length allPocketedBalls then
                    -- All balls pocketed are solids.
                    Grouped
                        { solids =
                            poolData.player
                        }

                else if List.length stripesPocketed == List.length allPocketedBalls then
                    -- All balls pocketed are stripes.
                    Grouped
                        { solids =
                            switchPlayer poolData.player
                        }

                else
                    poolData.target

            else
                poolData.target

        Grouped _ ->
            poolData.target


type alias BallPocketedEvents =
    { allPocketedBalls : List ( Time.Posix, ShotEvent )
    , solidsPocketed : List ( Time.Posix, ShotEvent )
    , stripesPocketed : List ( Time.Posix, ShotEvent )
    , scratched : Bool
    , eightBallPocketed : Bool
    }


{-| Categorize ball pocketing events.

TODO: It would be more efficient to fold over the list.

-}
groupPocketedEvents : List ( Time.Posix, ShotEvent ) -> BallPocketedEvents
groupPocketedEvents shotEvents =
    let
        allPocketedBalls =
            List.filter
                (\( shotTime, shotEvent ) ->
                    case shotEvent of
                        BallToPocket ball ->
                            True

                        CueHitBall ball ->
                            False

                        Scratch ->
                            True
                )
                shotEvents

        scratched =
            List.any
                (\( shotTime, shotEvent ) ->
                    case shotEvent of
                        BallToPocket ball ->
                            False

                        CueHitBall ball ->
                            False

                        Scratch ->
                            True
                )
                allPocketedBalls
    in
    { allPocketedBalls = allPocketedBalls
    , scratched = scratched
    , solidsPocketed = List.filter (ballPocketedInGroup SolidGroup) allPocketedBalls
    , stripesPocketed = List.filter (ballPocketedInGroup StripeGroup) allPocketedBalls
    , eightBallPocketed = List.any (ballPocketedInGroup EightGroup) allPocketedBalls
    }


updatePocketed : BallPocketedEvents -> PoolData -> List ( Ball, Player )
updatePocketed ballPocketedEvents poolData =
    let
        newPocketedBalls =
            (ballPocketedEvents.solidsPocketed
                ++ ballPocketedEvents.stripesPocketed
            )
                |> List.filterMap
                    (\( _, shotEvent ) ->
                        case shotEvent of
                            BallToPocket ball ->
                                Just ( ball, poolData.player )

                            _ ->
                                Nothing
                    )
    in
    List.append poolData.pocketed
        newPocketedBalls


type BallGroup
    = SolidGroup
    | StripeGroup
    | EightGroup


ballGroup : Ball -> BallGroup
ballGroup (Ball number group) =
    group


ballPocketedInGroup : BallGroup -> ( Time.Posix, ShotEvent ) -> Bool
ballPocketedInGroup ballGroup_ ( posixTime, shotEvent ) =
    case shotEvent of
        BallToPocket ball ->
            ballGroup ball == ballGroup_

        CueHitBall ball ->
            False

        Scratch ->
            False


{-| At the beginning of the shot...
-}
isValidHit : List ( Time.Posix, ShotEvent ) -> PoolData -> Bool
isValidHit shotEvents poolData =
    True


checkShot : List ( Time.Posix, ShotEvent ) -> BallPocketedEvents -> PoolData -> WhatHappened
checkShot shotEvents ballPocketedEvents poolData =
    if ballPocketedEvents.eightBallPocketed then
        Debug.todo "Handle game over."

    else if ballPocketedEvents.scratched then
        PlayersFault
            (Pool
                { poolData
                    | player = switchPlayer poolData.player
                }
            )

    else
        NextShot <|
            Pool
                { poolData
                    | player = checkNextPlayer ballPocketedEvents poolData
                }


{-| Check who should be the next player.
-}
checkNextPlayer : BallPocketedEvents -> PoolData -> Player
checkNextPlayer ({ allPocketedBalls, eightBallPocketed, solidsPocketed, stripesPocketed, scratched } as ballPocketedEvents) poolData =
    if scratched then
        switchPlayer poolData.player

    else if List.length allPocketedBalls == 0 then
        switchPlayer poolData.player

    else
        case currentTarget (Pool poolData) of
            OpenTable ->
                if pocketedInSameGroup ballPocketedEvents then
                    poolData.player

                else
                    switchPlayer poolData.player

            Solids ->
                if List.all (ballPocketedInGroup SolidGroup) allPocketedBalls then
                    poolData.player

                else
                    switchPlayer poolData.player

            Stripes ->
                if List.all (ballPocketedInGroup StripeGroup) allPocketedBalls then
                    poolData.player

                else
                    switchPlayer poolData.player

            EightBall ->
                if eightBallPocketed then
                    poolData.player

                else
                    switchPlayer poolData.player


pocketedInSameGroup : BallPocketedEvents -> Bool
pocketedInSameGroup { solidsPocketed, stripesPocketed } =
    (List.length solidsPocketed
        > 0
        && List.length stripesPocketed
        == 0
    )
        || (List.length stripesPocketed
                > 0
                && List.length solidsPocketed
                == 0
           )


{-| When a player shoots, but sends no events, we still want to log the event, so we try to find the last event time. If there is none, default to `Time.millisToPosix 0`.
-}
lastEventTime : List EventData -> Time.Posix
lastEventTime events =
    let
        maybeLastEventTime =
            events
                |> List.sortWith eventTimeComparison
                |> List.reverse
                |> List.head
                |> Maybe.andThen lastEventTimeByEventType
    in
    case maybeLastEventTime of
        Nothing ->
            Time.millisToPosix 0

        Just time ->
            time


lastEventTimeByEventType : EventData -> Maybe Time.Posix
lastEventTimeByEventType eventData =
    case eventData.event of
        Shot shotEvents ->
            lastShotEventTime shotEvents

        Racked ->
            Just eventData.when

        BallPlacedBehindHeadString ->
            Just eventData.when


lastShotEventTime : List ( Time.Posix, ShotEvent ) -> Maybe Time.Posix
lastShotEventTime shotEvents =
    case
        shotEvents
            |> List.sortWith shotEventTimeComparison
            |> List.reverse
    of
        [] ->
            Nothing

        ( firstShotTime, firstShotEvent ) :: otherShotEvents ->
            Just firstShotTime
