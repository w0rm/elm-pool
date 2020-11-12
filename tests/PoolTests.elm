module PoolTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pool
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Pool"
        [ describe "init"
            [ skip <|
                test "next action is to rack all the balls"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                        in
                        Expect.fail "How do I test this?"
                    )
            ]
        , describe "update"
            [ describe "playerShot"
                [ test "after player shoots cue into another ball, player wins!"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot
                                        [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall
                                        ]
                        in
                        case nextAction of
                            Pool.GameOver _ ->
                                Expect.pass

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "no events sent, still current player's turn"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot []
                        in
                        case nextAction of
                            Pool.NextShot _ ->
                                -- TODO: Inspect current player, should still be player 1.
                                Expect.pass

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
                                        []
                        in
                        case nextAction of
                            Pool.NextShot _ ->
                                -- TODO: Inspect current player, should be player 2.
                                Expect.pass

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        ]


type Player
    = Player1
    | Player2
