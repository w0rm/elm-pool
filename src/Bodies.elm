module Bodies exposing (Id(..))

import EightBall exposing (Ball)


type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Cushion
    | Pocket
