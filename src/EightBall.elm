module EightBall exposing
    ( Pool, AwaitingRack, AwaitingNextShot, AwaitingBallInHand, AwaitingPlaceBallBehindHeadstring, AwaitingNewGame, start
    , currentPlayer, currentScore
    , CurrentTarget(..), currentTarget
    , rack, ballPlacedBehindHeadString, ballPlacedInHand, playerShot
    , ShotEvent
    , cueHitBall, cueHitWall, ballFellInPocket, ballHitWall, scratch
    , Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall, ballNumber
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

@docs currentPlayer, currentScore

@docs CurrentTarget, currentTarget


# Update

@docs rack, ballPlacedBehindHeadString, ballPlacedInHand, playerShot


## Shot Events

@docs ShotEvent
@docs cueHitBall, cueHitWall, ballFellInPocket, ballHitWall, scratch


## Balls

@docs Ball, oneBall, twoBall, threeBall, fourBall, fiveBall, sixBall, sevenBall, eightBall, nineBall, tenBall, elevenBall, twelveBall, thirteenBall, fourteenBall, fifteenBall, numberedBall, ballNumber


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


{-| A numbered ball.
-}
type Ball
    = Ball Int BallGroup


{-| Create a ball.
-}
oneBall : Ball
oneBall =
    Ball 1 SolidGroup


{-| Create a ball.
-}
twoBall : Ball
twoBall =
    Ball 2 SolidGroup


{-| Create a ball.
-}
threeBall : Ball
threeBall =
    Ball 3 SolidGroup


{-| Create a ball.
-}
fourBall : Ball
fourBall =
    Ball 4 SolidGroup


{-| Create a ball.
-}
fiveBall : Ball
fiveBall =
    Ball 5 SolidGroup


{-| Create a ball.
-}
sixBall : Ball
sixBall =
    Ball 6 SolidGroup


{-| Create a ball.
-}
sevenBall : Ball
sevenBall =
    Ball 7 SolidGroup


{-| Create a ball.
-}
eightBall : Ball
eightBall =
    Ball 8 EightGroup


{-| Create a ball.
-}
nineBall : Ball
nineBall =
    Ball 9 StripeGroup


{-| Create a ball.
-}
tenBall : Ball
tenBall =
    Ball 10 StripeGroup


{-| Create a ball.
-}
elevenBall : Ball
elevenBall =
    Ball 11 StripeGroup


{-| Create a ball.
-}
twelveBall : Ball
twelveBall =
    Ball 12 StripeGroup


{-| Create a ball.
-}
thirteenBall : Ball
thirteenBall =
    Ball 13 StripeGroup


{-| Create a ball.
-}
fourteenBall : Ball
fourteenBall =
    Ball 14 StripeGroup


{-| Create a ball.
-}
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


ballNumber : Ball -> Int
ballNumber (Ball n _) =
    n



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


{-| The current target.
-}
type CurrentTarget
    = OpenTable
    | Solids
    | Stripes
    | EightBall


{-| Get the current target based on the current player and pocketed balls in the game.

1.  Open table - when the player may shoot at either solids or stripes, attempting to pocket a ball of either set.
2.  Solids - the current player must shoot at solids, pocketing at least one of them without scratching in order to keep shooting.
3.  Stripes - the current player must shoot at stripes, pocketing at least one of them without scratching in order to keep shooting.
4.  8-ball - the player must shoot at the 8-ball. If it is pocketed without a foul or scratch, the player wins.

-}
currentTarget : Pool state -> CurrentTarget
currentTarget (Pool ({ pocketed } as poolData)) =
    case poolData.target of
        Open ->
            OpenTable

        Grouped { solids } ->
            if solids == poolData.player then
                if pocketedIn SolidGroup pocketed == 7 then
                    EightBall

                else
                    Solids

            else if pocketedIn StripeGroup pocketed == 7 then
                EightBall

            else
                Stripes



-- Update


{-| Waiting for the balls to be racked.
-}
type AwaitingRack
    = AwaitingRack


{-| Ready for a player to take another shot.
-}
type AwaitingNextShot
    = AwaitingNextShot


{-| When a player scratches, or otherwise fouls, during regular play, the next player is given ball-in-hand anywhere on the table.

See [WPA rules](https://wpapool.com/rules-of-play/) 1.5 Cue Ball in Hand for more info.

-}
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


{-| When the game is over, start a new game to play again.
-}
type AwaitingNewGame
    = AwaitingNewGame


{-| The balls must be racked before the player can place the cue ball and break.
-}
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
      -- Player actions
    | BallPlacedBehindHeadString
    | BallPlacedInHand
      -- | CallShot Ball Pocket
    | Shot (List ( Time.Posix, ShotEvent ))
      -- Game over
    | GameOver_


{-| All potential shot events available.

There are a few which should be supported, but are not yet:

  - BallOffTable
  - BallToBall
  - BallToWall

-}
type ShotEvent
    = --       -- | BallOffTable Ball
      -- BallToBall Ball Ball (List Ball)
      BallToPocket Ball --Pocket
    | BallToWall Ball -- Wall
    | CueHitBall Ball
    | CueHitWall
    | Scratch


{-| When the ball is placed behind the head string after racking.
-}
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


{-| When the ball is placed anywhere on the table.
-}
ballPlacedInHand : Time.Posix -> Pool AwaitingBallInHand -> Pool AwaitingNextShot
ballPlacedInHand when (Pool data) =
    Pool
        { data
            | events =
                data.events
                    ++ [ { when = when
                         , event = BallPlacedInHand
                         }
                       ]
        }


type Event
    = Event EventData


{-| When the cue ball comes into contact with a numbered ball.

Note: once they are touching, there's no need to send this event again unless they are separated and come back into contact.

-}
cueHitBall : Time.Posix -> Ball -> ( Time.Posix, ShotEvent )
cueHitBall when ball =
    ( when
    , CueHitBall ball
    )


{-| When the cue ball touches the wall.

Note: once they are touching, there's no need to send this event again unless they are separated and come back into contact.

-}
cueHitWall : Time.Posix -> ( Time.Posix, ShotEvent )
cueHitWall when =
    ( when
    , CueHitWall
    )



-- {-| When two or more balls contact one another.
--
-- Note: once they are touching, there's no need to send this event again unless they are separated and come back into contact.
--
-- -}
-- ballsCollided : Time.Posix -> Ball -> Ball -> List Ball -> ( Time.Posix, ShotEvent )
-- ballsCollided when ball1 ball2 otherBalls =
--     ( when
--     , BallToBall ball1 ball2 otherBalls
--     )


{-| When a ball touches the wall.

Note: once they are touching, there's no need to send this event again unless they are separated and come back into contact.

-}
ballHitWall : Time.Posix -> Ball -> ( Time.Posix, ShotEvent )
ballHitWall when ball =
    ( when
    , BallToWall ball
    )


{-| When a numbered ball is pocketed.
-}
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


{-| After a player shoots, this returns the outcome.

  - PlayersFault - when a player scratches (or, in the future, hits the wrong ball first). The next player must place the ball in hand.
  - NextShot - waiting for a player to shoot. Use `currentPlayer` to figure out which player is shooting. Use `playerShot` after the player shoots.
  - GameOver - when the game is over, this returns the winner.
  - Error - when something unexpected happens, like a ball pocketed twice.

-}
type WhatHappened
    = PlayersFault (Pool AwaitingBallInHand)
    | NextShot (Pool AwaitingNextShot)
      -- | NextTurn (Pool AwaitingNextTurn)
    | GameOver (Pool AwaitingNewGame) { winner : Int }
    | Error String


{-| Set game over via this function so we don't forget to add the internal event.
-}
gameOver : Player -> PoolData -> WhatHappened
gameOver winner poolData =
    GameOver
        (Pool
            { poolData
                | events =
                    poolData.events
                        ++ [ { event = GameOver_
                             , when = lastEventTime poolData.events
                             }
                           ]
            }
        )
        { winner = playerToInt winner
        }


{-| Send a series of shot events.

Note: if no balls are hit by the cue ball, send an empty list.

    playerShot [] pool -- Cue struck, but no other balls hit.

-}
playerShot : List ( Time.Posix, ShotEvent ) -> Pool AwaitingNextShot -> WhatHappened
playerShot shotEvents (Pool data) =
    case shotEvents of
        [] ->
            -- Assume the cue is struck, but no other balls are hit.
            PlayersFault <|
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

                previousTarget =
                    -- This gets the currentTarget on the _old_ `data`.
                    currentTarget (Pool data)
            in
            checkShot shotEvents
                ballPocketedEvents
                previousTarget
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

                        BallToWall _ ->
                            False

                        CueHitBall _ ->
                            False

                        CueHitWall ->
                            False

                        Scratch ->
                            True
                )
                shotEvents

        scratched =
            List.any
                (\( shotTime, shotEvent ) ->
                    case shotEvent of
                        BallToPocket _ ->
                            False

                        BallToWall _ ->
                            False

                        CueHitBall _ ->
                            False

                        CueHitWall ->
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
    poolData.pocketed
        ++ newPocketedBalls


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

        BallToWall _ ->
            False

        CueHitBall ball ->
            False

        CueHitWall ->
            False

        Scratch ->
            False


{-| Check whether the player makes a legal hit.

A legal hit during regular play (after the break) is based on the target ball group:

  - Open table: the first ball hit by the cue can be either a solid or a stripe, but not the 8-ball.
  - Solid group: the first ball hit by the cue must be in the solid group.
  - Stripe group: the first ball hit by the cue must be in the stripe group.
  - Eight ball: the first ball hit by the cue must be the 8-ball.

-}
isLegalHit : List ( Time.Posix, ShotEvent ) -> CurrentTarget -> Bool
isLegalHit shotEvents previousTarget =
    case ( previousTarget, legalFirstBallHitGroup shotEvents ) of
        ( OpenTable, Just SolidGroup ) ->
            True

        ( OpenTable, Just StripeGroup ) ->
            True

        ( Solids, Just SolidGroup ) ->
            True

        ( Stripes, Just StripeGroup ) ->
            True

        ( EightBall, Just EightGroup ) ->
            True

        _ ->
            False


{-| This finds the group of the first ball hit by the cue, if there is one, and only if any ball hit a wall after.
-}
legalFirstBallHitGroup : List ( Time.Posix, ShotEvent ) -> Maybe BallGroup
legalFirstBallHitGroup shotEvents =
    case shotEvents of
        [] ->
            Nothing

        ( _, CueHitBall ball ) :: otherShotEvents ->
            if List.any hasHitAWallOrPocket otherShotEvents then
                Just (ballGroup ball)

            else
                Nothing

        firstShotEvent :: otherShotEvents ->
            legalFirstBallHitGroup otherShotEvents


hasHitAWallOrPocket : ( Time.Posix, ShotEvent ) -> Bool
hasHitAWallOrPocket ( _, shotEvent ) =
    case shotEvent of
        CueHitBall _ ->
            False

        CueHitWall ->
            True

        BallToPocket _ ->
            True

        BallToWall _ ->
            True

        Scratch ->
            False


checkShot : List ( Time.Posix, ShotEvent ) -> BallPocketedEvents -> CurrentTarget -> PoolData -> WhatHappened
checkShot shotEvents ballPocketedEvents previousTarget poolData =
    if ballPocketedEvents.eightBallPocketed then
        case currentTarget (Pool poolData) of
            EightBall ->
                -- TODO: Combine case into tuple with new type `Scratched | NotScratched`.
                let
                    winningPlayer =
                        if ballPocketedEvents.scratched || not (isLegalHit shotEvents previousTarget) then
                            switchPlayer poolData.player

                        else
                            poolData.player
                in
                gameOver winningPlayer poolData

            _ ->
                -- If the player wasn't targeting the 8-ball, then they lose!
                gameOver (switchPlayer poolData.player) poolData

    else if ballPocketedEvents.scratched || not (isLegalHit shotEvents previousTarget) then
        PlayersFault
            (Pool
                { poolData
                    | player = switchPlayer poolData.player

                    -- TODO: Log Scratched/PlayersFault internal event.
                    -- Should these be two separate events?
                }
            )

    else
        NextShot <|
            Pool
                { poolData
                    | player =
                        checkNextPlayer ballPocketedEvents
                            previousTarget
                            poolData
                }


{-| Check who should be the next player.
-}
checkNextPlayer : BallPocketedEvents -> CurrentTarget -> PoolData -> Player
checkNextPlayer ({ allPocketedBalls, eightBallPocketed, solidsPocketed, stripesPocketed, scratched } as ballPocketedEvents) previousTarget poolData =
    if scratched then
        switchPlayer poolData.player

    else if List.length allPocketedBalls == 0 then
        switchPlayer poolData.player

    else
        case previousTarget of
            OpenTable ->
                if pocketedInSameGroup ballPocketedEvents then
                    poolData.player

                else
                    switchPlayer poolData.player

            Solids ->
                if List.length solidsPocketed > 0 then
                    poolData.player

                else
                    switchPlayer poolData.player

            Stripes ->
                if List.length stripesPocketed > 0 then
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

        BallPlacedInHand ->
            Just eventData.when

        GameOver_ ->
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
