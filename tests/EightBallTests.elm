module EightBallTests exposing (..)

import EightBall
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Pool"
        [ describe "view"
            [ describe "currentScore"
                [ test "no events sent, score is 0-0"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "a ball is pocketed and target balls are decided, score is 1-0"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 1
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "several balls are pocketed, score is 3-5"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 3) EightBall.fiveBall
                                        ]
                                    |> andKeepShooting []
                                    -- Player 2 starts shooting
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 5) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.tenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.elevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.twelveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.thirteenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 3
                                        , player2 = 5
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        , describe "update"
            [ describe "playerShot"
                [ test "after player shoots cue hits nothing, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits ball, but doesn't pocket it, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 0) EightBall.twoBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, and next player hits nothing, back to first"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                                    |> andKeepShooting []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a ball, that group becomes the player's target"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.Solids

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets two stripes, then that player's target will become stripes"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fifteenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.tenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fifteenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.Stripes

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a solid and two strips, table is still open"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.tenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.OpenTable

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a ball but also scratches, table is still open"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 1)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        , EightBall.currentPlayer >> Expect.equal 1
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        ]


andKeepShooting : List ( Time.Posix, EightBall.ShotEvent ) -> EightBall.WhatHappened -> EightBall.WhatHappened
andKeepShooting shotEvents ruling =
    case ruling of
        EightBall.NextShot pool ->
            EightBall.playerShot shotEvents pool

        _ ->
            ruling
