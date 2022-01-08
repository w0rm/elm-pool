module Temperature.Interval exposing
    ( Interval
    , from, fromEndpoints, singleton
    , union, intersection
    , hull, hullN, hullOf, hullOfN
    , aggregate, aggregateN, aggregateOf, aggregateOfN
    , endpoints, minValue, maxValue, midpoint, width
    , contains, isContainedIn, intersects, isSingleton
    , interpolate, interpolationParameter
    , add, subtract
    , plus, minus
    )

{-| This module behaves much like [`Quantity.Interval`](Quantity-Interval), but
works on [`Temperature`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Temperature)
values.

@docs Interval


# Constructors

@docs from, fromEndpoints, singleton


## Booleans

@docs union, intersection


## Hull

These functions let you construct an `Interval` containing one or more input
temperatures.

@docs hull, hullN, hullOf, hullOfN


## Aggregation

These functions let you 'aggregate' one or more temperature intervals into a
single larger interval that contains all of them.

@docs aggregate, aggregateN, aggregateOf, aggregateOfN


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Queries

@docs contains, isContainedIn, intersects, isSingleton


# Interpolation

@docs interpolate, interpolationParameter


# Arithmetic

@docs add, subtract

The `plus` and `minus` functions both involve `Interval Float CelsiusDegrees`
values, where `Interval` in this case refers to the type in the
`Quantity.Interval` module. This represents an interval of [temperature deltas](Temperatur#Delta).

@docs plus, minus

-}

import Float.Extra as Float
import Quantity exposing (Quantity)
import Quantity.Interval as Interval
import Temperature exposing (CelsiusDegrees, Temperature)


{-| Represents a finite, closed interval with a minimum and maximum temperature,
for example the interval from 20 to 30 degrees Celsius.
-}
type Interval
    = Interval ( Temperature, Temperature )


{-| Construct a zero-width interval containing a single temperature.
-}
singleton : Temperature -> Interval
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum
temperatures of the interval). The two values should be given in order but will
be swapped if necessary to ensure a valid interval is returned.
-}
fromEndpoints : ( Temperature, Temperature ) -> Interval
fromEndpoints givenEndpoints =
    let
        ( firstValue, secondValue ) =
            givenEndpoints
    in
    if firstValue |> Temperature.lessThanOrEqualTo secondValue then
        Interval givenEndpoints

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval with the two given endpoints (which can be provided in
either order).
-}
from : Temperature -> Temperature -> Interval
from firstValue secondValue =
    if firstValue |> Temperature.lessThanOrEqualTo secondValue then
        Interval ( firstValue, secondValue )

    else
        Interval ( secondValue, firstValue )


{-| Construct an interval containing one or more input temperatures.
-}
hull : Temperature -> List Temperature -> Interval
hull first rest =
    hullHelp first first rest


hullHelp : Temperature -> Temperature -> List Temperature -> Interval
hullHelp low high remaining =
    case remaining of
        first :: rest ->
            hullHelp
                (Temperature.min first low)
                (Temperature.max first high)
                rest

        [] ->
            from low high


{-| Construct an interval containing all temperatures in the given list. If the
list is empty, returns `Nothing`.
-}
hullN : List Temperature -> Maybe Interval
hullN values =
    case values of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
`Temperature` can be extracted from it:

    type alias WeatherMeasurement =
        { windSpeed : Speed
        , temperature : Temperature
        , barometricPressure : Pressure
        }

    temperatureRange =
        Temperature.Interval.hullOf .temperature <|
            measurement1
            [ measurement2
            , measurement3
            , measurement4
            ]

-}
hullOf : (a -> Temperature) -> a -> List a -> Interval
hullOf getTemperature first rest =
    let
        firstTemperature =
            getTemperature first
    in
    hullOfHelp firstTemperature firstTemperature getTemperature rest


hullOfHelp : Temperature -> Temperature -> (a -> Temperature) -> List a -> Interval
hullOfHelp low high getTemperature remaining =
    case remaining of
        first :: rest ->
            let
                temperature =
                    getTemperature first
            in
            hullOfHelp
                (Temperature.min temperature low)
                (Temperature.max temperature high)
                getTemperature
                rest

        [] ->
            from low high


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Temperature) -> List a -> Maybe Interval
hullOfN getTemperature items =
    case items of
        first :: rest ->
            Just (hullOf getTemperature first rest)

        [] ->
            Nothing


{-| Construct an interval containing both of the given intervals.
-}
union : Interval -> Interval -> Interval
union firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    Interval ( Temperature.min min1 min2, Temperature.max max1 max2 )


{-| Attempt to construct an interval containing all the temperatures common to
both given intervals. If the intervals do not intersect, returns `Nothing`. If
the two intervals just touch, a singleton interval will be returned.
-}
intersection : Interval -> Interval -> Maybe Interval
intersection firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval

        maxOfMins =
            Temperature.max min1 min2

        minOfMaxes =
            Temperature.min max1 max2
    in
    if maxOfMins |> Temperature.lessThanOrEqualTo minOfMaxes then
        Just (Interval ( maxOfMins, minOfMaxes ))

    else
        Nothing


{-| Construct an interval containing one or more given intervals.
-}
aggregate : Interval -> List Interval -> Interval
aggregate first rest =
    List.foldl union first rest


{-| Construct an interval containing all of the intervals in the given list. If
the list is empty, returns `Nothing`.
-}
aggregateN : List Interval -> Maybe Interval
aggregateN intervals =
    case intervals of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as a temperature interval can be generated from it (similar to [`hullOf`](#hullOf)).
-}
aggregateOf : (a -> Interval) -> a -> List a -> Interval
aggregateOf getInterval first rest =
    List.foldl
        (\item accumulated ->
            union accumulated (getInterval item)
        )
        (getInterval first)
        rest


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> Interval) -> List a -> Maybe Interval
aggregateOfN getInterval items =
    case items of
        first :: rest ->
            Just (aggregateOf getInterval first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum temperatures) as a
tuple. The first temperature will always be less than or equal to the second.
-}
endpoints : Interval -> ( Temperature, Temperature )
endpoints (Interval intervalEndpoints) =
    intervalEndpoints


{-| Get the minimum temperature of an interval.
-}
minValue : Interval -> Temperature
minValue interval =
    Tuple.first (endpoints interval)


{-| Get the maximum temperature of an interval.
-}
maxValue : Interval -> Temperature
maxValue interval =
    Tuple.second (endpoints interval)


{-| Get the midpoint of an interval.
-}
midpoint : Interval -> Temperature
midpoint interval =
    minValue interval |> Temperature.plus (Quantity.half (width interval))


{-| Get the width of an interval. This will never be negative. Note that this
returns a [`Temperature.Delta`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Temperature#Delta),
not a `Temperature`.
-}
width : Interval -> Temperature.Delta
width interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMaxValue |> Temperature.minus intervalMinValue


{-| Interpolate between an interval's endpoints; a value of 0.0 corresponds to
the minimum temperature of the interval, a value of 0.5 corresponds to its
midpoint and a value of 1.0 corresponds to its maximum temperature. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.
-}
interpolate : Interval -> Float -> Temperature
interpolate interval t =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    Temperature.kelvins <|
        Float.interpolateFrom
            (Temperature.inKelvins intervalMinValue)
            (Temperature.inKelvins intervalMaxValue)
            t


{-| Given an interval and a given value, determine the corresponding
interpolation parameter (the parameter that you would pass to [`interpolate`](#interpolate)
to get the given value).
-}
interpolationParameter : Interval -> Temperature -> Float
interpolationParameter (Interval ( low, high )) temperature =
    if low |> Temperature.lessThan high then
        Quantity.ratio
            (temperature |> Temperature.minus low)
            (high |> Temperature.minus low)

    else if temperature |> Temperature.lessThan low then
        -1 / 0

    else if temperature |> Temperature.greaterThan high then
        1 / 0

    else
        -- temperature, low and high are all equal
        0


{-| Check if an interval contains a given temperature. The minimum and maximum
temperatures of the interval are considered to be contained in the interval.
-}
contains : Temperature -> Interval -> Bool
contains value interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    (intervalMinValue |> Temperature.lessThanOrEqualTo value)
        && (value |> Temperature.lessThanOrEqualTo intervalMaxValue)


{-| Check if two intervals touch or overlap (have any temperatures in common).
Intervals that just touch each other are considered to intersect.
-}
intersects : Interval -> Interval -> Bool
intersects firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min1 |> Temperature.lessThanOrEqualTo max2)
        && (max1 |> Temperature.greaterThanOrEqualTo min2)


{-| Check if the second interval is fully contained in the first.
-}
isContainedIn : Interval -> Interval -> Bool
isContainedIn firstInterval secondInterval =
    let
        ( min1, max1 ) =
            endpoints firstInterval

        ( min2, max2 ) =
            endpoints secondInterval
    in
    (min2 |> Temperature.greaterThanOrEqualTo min1)
        && (max2 |> Temperature.lessThanOrEqualTo max1)


{-| Check if the interval is a singleton (the minimum and maximum temperatures
are the same).
-}
isSingleton : Interval -> Bool
isSingleton interval =
    let
        ( intervalMinValue, intervalMaxValue ) =
            endpoints interval
    in
    intervalMinValue == intervalMaxValue


{-| Add a [temperature delta](Temperature#Delta) to a temperature interval.
-}
add : Temperature.Delta -> Interval -> Interval
add delta (Interval ( low, high )) =
    Interval
        ( low |> Temperature.plus delta
        , high |> Temperature.plus delta
        )


{-| Subtract a [temperature delta](Temperature#Delta) from a temperature
interval.
-}
subtract : Temperature.Delta -> Interval -> Interval
subtract delta interval =
    add (Quantity.negate delta) interval


{-| Add a temperature delta interval to a temperature interval to give a new
temperature interval:

    temperatureInterval =
        Temperature.Interval.from
            (Temperature.degreesCelsius 20)
            (Temperature.degreesCelsius 25)

    deltaInterval =
        Quantity.Interval.from
            (Temperature.celsiusDegrees -1)
            (Temperature.celsiusDegrees 4)

    temperatureInterval
        |> Temperature.Interval.plus deltaInterval
    --> Temperature.Interval.from
    -->     (Temperature.degreesCelsius 19)
    -->     (Temperature.degreesCelsius 29)

-}
plus : Interval.Interval Float CelsiusDegrees -> Interval -> Interval
plus delta (Interval ( low, high )) =
    from
        (low |> Temperature.plus (Interval.minValue delta))
        (high |> Temperature.plus (Interval.maxValue delta))


{-| Subtract the first given temperature interval from the second, resulting in
a temperature delta interval:

    firstInterval =
        Temperature.Interval.from
            (Temperature.degreesCelsius 5)
            (Temperature.degreesCelsius 10)

    secondInterval =
        Temperature.Interval.from
            (Temperature.degreesCelsius 30)
            (Temperature.degreesCelsius 40)

    secondInterval
        |> Temperature.Interval.minus firstInterval
    --> Quantity.Interval.from
    -->     (Temperature.celsiusDegrees 20)
    -->     (Temperature.celsiusDegrees 35)

-}
minus : Interval -> Interval -> Interval.Interval Float CelsiusDegrees
minus (Interval ( a2, b2 )) (Interval ( a1, b1 )) =
    Interval.from
        (a1 |> Temperature.minus b2)
        (b1 |> Temperature.minus a2)
