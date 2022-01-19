module Evergreen.V1.Bodies exposing (..)

import Evergreen.V1.EightBall


type Id
    = Floor
    | Numbered Evergreen.V1.EightBall.Ball
    | CueBall
    | Table
    | Walls