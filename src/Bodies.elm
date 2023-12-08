module Bodies exposing (Id(..))

{-|

@docs Id

-}

import EightBall exposing (Ball)


{-| Identify the different bodies in the physical simulation
-}
type Id
    = Floor
    | Numbered Ball
    | CueBall
    | Table
    | Cushion
    | Pocket
