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
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
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
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
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
                                        , EightBall.ballHitWall (Time.millisToPosix 1) EightBall.twoBall
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
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
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
                                    |> Expect.all
                                        [ EightBall.currentTarget >> Expect.equal EightBall.Solids
                                        , EightBall.currentPlayer >> Expect.equal 0
                                        ]

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
                , test "after player shoots cue and pockets all of their target balls, then on the next shot targeting the 8-ball, it's still their shot!"
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
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sixBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sevenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 0
                                        , EightBall.currentTarget >> Expect.equal EightBall.EightBall
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets all of their solid balls, then on the next shot targeting the 8-ball, it's still their shot!"
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
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.threeBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sixBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sevenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 0
                                        , EightBall.currentTarget >> Expect.equal EightBall.EightBall
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets all of their striped balls, then on the next shot targeting the 8-ball, it's still their shot!"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.nineBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fifteenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fifteenBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.tenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.tenBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.elevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.elevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twelveBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.thirteenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.thirteenBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fourteenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourteenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 0
                                        , EightBall.currentTarget >> Expect.equal EightBall.EightBall
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets all of their balls, then hits 8-ball without scratching, they win!"
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
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sixBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sevenBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots back and forth, then one finishes all of their target, then hits 8-ball without scratching, they win!"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 makes 8-ball
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1800) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player finishes all of their target, then hits 8-ball but scratches, they lose :("
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 makes 8-ball, but then scratches
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1800) EightBall.eightBall
                                        , EightBall.scratch (Time.millisToPosix 1700)
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player finishes all of their target, then hits another ball before pocketing the 8-ball, they lose :("
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 makes 8-ball, but hit the 7-ball first!
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1800) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots the 8-ball early (before they have finished all of their target balls), they lose :("
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again and accidentally hits the 8-ball in early.
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            , describe "legal hit"
                [ test "if player has solids but shoots a stripe first, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.oneBall
                                        ]
                                    -- Player 1 is now solids, but they hit the 9-ball!
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.Stripes
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player has stripes but shoots a solid first, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.nineBall
                                        ]
                                    -- Player 1 is now stripes, but they hit the 2-ball!
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.Solids
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player is targeting the 8-ball but shoots any other ball first, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 accidentally hits the wrong ball first.
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.sevenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 0
                                        , EightBall.currentTarget >> Expect.equal EightBall.Stripes
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player does not make contact with a ball in their target group, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player hits a ball in their target group but does not hit a wall after, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall

                                        -- No wall hit.
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player hits the cue into a wall then into a ball in their target group, the other player gets ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitWall (Time.millisToPosix 1)
                                        , EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player hits a ball in their target group and then the target hits a wall after, the other player does not get ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballHitWall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "if player hits a ball in their target group and then the cue ball hits a wall after, the other player does not get ball-in-hand"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.cueHitWall (Time.millisToPosix 1)
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 1
                                        , EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            , describe "ballPlacedInHand"
                [ test "when player scratches, the other player must place ball in hand before continuing to shoot"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 789)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "when player scratches and next player places ball in hand, they may continue to shoot"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 789)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.ballPlacedInHand (Time.millisToPosix 800)
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

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
    let
        -- Could instead expose `EightBall.lastEventTime` for consistency.
        lastEventTime =
            shotEvents
                |> List.map (\( time, _ ) -> time)
                |> List.sortBy (Time.toMillis Time.utc)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault (Time.millisToPosix 0)
    in
    case ruling of
        EightBall.NextShot pool ->
            EightBall.playerShot shotEvents pool

        EightBall.PlayersFault pool ->
            pool
                |> EightBall.ballPlacedInHand lastEventTime
                |> EightBall.playerShot shotEvents

        EightBall.GameOver _ _ ->
            ruling

        EightBall.Error _ ->
            ruling
