module Angle.Interval exposing (sin, cos)

{-|

@docs sin, cos

-}

import Angle exposing (Radians)
import Quantity exposing (Quantity(..), Unitless)
import Quantity.Interval as Interval


type alias Interval =
    Interval.Interval Float Radians


{-| For a given range of angle values θ, determine the possible range of values
of sin(θ).

    Angle.Interval.sin <|
        Interval.from Quantity.zero (Angle.degrees 45)
    --> Interval.from Quantity.zero (Quantity.float 0.7071)

    Angle.Interval.sin <|
        Interval.from Quantity.zero (Angle.degrees 180)
    --> Interval.from Quantity.zero (Quantity.float 1)

-}
sin : Interval -> Interval.Interval Float Unitless
sin interval =
    if Interval.isSingleton interval then
        Interval.singleton (Quantity.float (Angle.sin (Interval.minValue interval)))

    else
        let
            ( includesMin, includesMax ) =
                sinIncludesMinMax interval

            ( a, b ) =
                Interval.endpoints interval

            newMin =
                if includesMin then
                    Quantity.float -1

                else
                    Quantity.float (min (Angle.sin a) (Angle.sin b))

            newMax =
                if includesMax then
                    Quantity.float 1

                else
                    Quantity.float (max (Angle.sin a) (Angle.sin b))
        in
        Interval.from newMin newMax


{-| For a given range of angle values θ, determine the possible range of values
of cos(θ).

    Angle.Interval.cos <|
        Interval.from Quantity.zero (Angle.degrees 45)
    --> Interval.from
    -->     (Quantity.float 0.7071)
    -->     (Quantity.float 1)

    Angle.Interval.cos <|
        Interval.from Quantity.zero (Angle.degrees 180)
    --> Interval.from
    -->     (Quantity.float -1)
    -->     (Quantity.float 1)

-}
cos : Interval -> Interval.Interval Float Unitless
cos interval =
    if Interval.isSingleton interval then
        Interval.singleton (Quantity.float (Angle.cos (Interval.minValue interval)))

    else
        let
            ( includesMin, includesMax ) =
                cosIncludesMinMax interval

            ( a, b ) =
                Interval.endpoints interval

            newMin =
                if includesMin then
                    Quantity.float -1

                else
                    Quantity.float (min (Angle.cos a) (Angle.cos b))

            newMax =
                if includesMax then
                    Quantity.float 1

                else
                    Quantity.float (max (Angle.cos a) (Angle.cos b))
        in
        Interval.from newMin newMax


{-| cos(x - pi/2) = sin(x), therefore if cos(interval - pi/2) includes
the maximum/minimum, that means sin(interval) includes the maximum/minimum
accordingly.
-}
sinIncludesMinMax : Interval -> ( Bool, Bool )
sinIncludesMinMax interval =
    interval |> Interval.subtract (Angle.radians (pi / 2)) |> cosIncludesMinMax


{-| cos(x + pi) = -cos(x), therefore if cos(interval + pi) includes the maximum,
that means cos(interval) includes the minimum.
-}
cosIncludesMinMax : Interval -> ( Bool, Bool )
cosIncludesMinMax interval =
    ( interval |> Interval.add (Angle.radians pi) |> cosIncludesMax
    , interval |> cosIncludesMax
    )


{-| The maximum of cos(x) is x = 2 pi \* k for every integer k.
If `minValue` and `maxValue` are in different branches
(meaning diffrent values of k), then the interval must pass through
2 pi \* k, which means the interval must include the maximum value.
-}
cosIncludesMax : Interval -> Bool
cosIncludesMax interval =
    let
        ( a, b ) =
            Interval.endpoints interval

        twoPi =
            Angle.radians (2 * pi)

        minBranch =
            floor (Quantity.ratio a twoPi)

        maxBranch =
            floor (Quantity.ratio b twoPi)
    in
    minBranch /= maxBranch
