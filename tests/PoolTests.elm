module PoolTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pool
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Pool"
        [ describe "Format8Ball"
            [ describe "init"
                [ describe "eightball"
                    [ test "next action is to rack all the balls"
                        (\_ ->
                            let
                                nextAction =
                                    Pool.eightBall Player1 Player2
                                        |> Pool.nextAction
                            in
                            case nextAction of
                                Just (Pool.Rack balls) ->
                                    balls
                                        |> Expect.equalLists
                                            [ Pool.Numbered 1
                                            , Pool.Numbered 2
                                            , Pool.Numbered 3
                                            , Pool.Numbered 4
                                            , Pool.Numbered 5
                                            , Pool.Numbered 6
                                            , Pool.Numbered 7
                                            , Pool.Numbered 8
                                            , Pool.Numbered 9
                                            , Pool.Numbered 10
                                            , Pool.Numbered 11
                                            , Pool.Numbered 12
                                            , Pool.Numbered 13
                                            , Pool.Numbered 14
                                            , Pool.Numbered 15
                                            ]

                                _ ->
                                    Expect.fail "Should be Rack [1-15]"
                        )
                    ]
                ]
            , describe "update"
                [ describe "nextAction"
                    [ test "after rack next action is to place ball in the kitchen"
                        (\_ ->
                            let
                                nextAction =
                                    Pool.eightBall Player1 Player2
                                        |> Pool.update
                                            [ ( Time.millisToPosix 0
                                              , Pool.Rack
                                                    [ Pool.Numbered 1
                                                    , Pool.Numbered 2
                                                    , Pool.Numbered 3
                                                    , Pool.Numbered 4
                                                    , Pool.Numbered 5
                                                    , Pool.Numbered 6
                                                    , Pool.Numbered 7
                                                    , Pool.Numbered 8
                                                    , Pool.Numbered 9
                                                    , Pool.Numbered 10
                                                    , Pool.Numbered 11
                                                    , Pool.Numbered 12
                                                    , Pool.Numbered 13
                                                    , Pool.Numbered 14
                                                    , Pool.Numbered 15
                                                    ]
                                              )
                                            ]
                                        |> Pool.nextAction
                            in
                            case nextAction of
                                Just (Pool.PlaceBallInKitchen player) ->
                                    player
                                        |> Expect.equal Player1

                                _ ->
                                    Expect.fail "Should be PlaceBallInKitchen"
                        )
                    ]
                ]
            ]
        ]


type Player
    = Player1
    | Player2
