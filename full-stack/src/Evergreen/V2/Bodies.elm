module Evergreen.V2.Bodies exposing (..)

import Evergreen.V2.EightBall


type Id
    = Floor
    | Numbered Evergreen.V2.EightBall.Ball
    | CueBall
    | Table
    | Walls