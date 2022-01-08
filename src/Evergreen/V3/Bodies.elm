module Evergreen.V3.Bodies exposing (..)

import Evergreen.V3.EightBall


type Id
    = Floor
    | Numbered Evergreen.V3.EightBall.Ball
    | CueBall
    | Table
    | Walls