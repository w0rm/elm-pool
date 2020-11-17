module PoolTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pool
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Pool"
        [ describe "update"
            [ describe "playerShot"
                [ test "no events sent, still current player's turn"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot []
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot
                                        [ Pool.cueStruck (Time.millisToPosix 0)
                                        ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, and next player hits nothing, back to first"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueStruck (Time.millisToPosix 0) ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 1) ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue into another ball twice, player wins!"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot
                                        [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall
                                        , Pool.cueHitBall (Time.millisToPosix 2) Pool.oneBall
                                        ]
                        in
                        case nextAction of
                            Pool.GameOver pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue into another ball, but then misses on next shot, next player must shoot twice to win"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 2) ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 3) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 5) Pool.oneBall ]
                        in
                        case nextAction of
                            Pool.GameOver pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        ]


andKeepShooting : List ( Time.Posix, Pool.ShotEvent ) -> Pool.WhatHappened -> Pool.WhatHappened
andKeepShooting shotEvents ruling =
    case ruling of
        Pool.NextShot pool ->
            Pool.playerShot shotEvents pool

        _ ->
            ruling
