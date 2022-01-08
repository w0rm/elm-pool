module Quantity.Interval exposing
    ( Interval
    , from, fromEndpoints, singleton
    , union, intersection
    , hull, hullN, hullOf, hullOfN, hull3
    , aggregate, aggregateN, aggregateOf, aggregateOfN, aggregate3
    , endpoints, minValue, maxValue, midpoint, width
    , contains, isContainedIn, intersects, isSingleton
    , interpolate, interpolationParameter
    , negate, add, subtract, multiplyBy, divideBy, half, twice
    , plus, minus, times, abs, squared, cubed
    , randomValue
    )

{-| This modules contains most of the core functionality for creating and
working with `Interval` values.

Note: most examples assume that you have imported this module as

    import Quantity.Interval as Interval

@docs Interval


# Constructors

@docs from, fromEndpoints, singleton


## Booleans

@docs union, intersection


## Hull

These functions let you construct an `Interval` containing one or more input
values.

@docs hull, hullN, hullOf, hullOfN, hull3


## Aggregation

These functions let you 'aggregate' one or more intervals into a single larger
interval that contains all of them.

@docs aggregate, aggregateN, aggregateOf, aggregateOfN, aggregate3


# Properties

@docs endpoints, minValue, maxValue, midpoint, width


# Queries

@docs contains, isContainedIn, intersects, isSingleton


# Interpolation

@docs interpolate, interpolationParameter


# Arithmetic

These functions let you do math with `Interval` values, following the rules of
[interval arithmetic](https://en.wikipedia.org/wiki/Interval_arithmetic).

@docs negate, add, subtract, multiplyBy, divideBy, half, twice
@docs plus, minus, times, abs, squared, cubed


# Random value generation

@docs randomValue

-}

import Quantity exposing (Cubed, Product, Quantity(..), Squared)
import Random exposing (Generator)


{-| Represents a finite, closed interval with a minimum and maximum value.

The two type parameters match those of the [`Quantity`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity)
type from `elm-units`. For example, an `Interval Int Pixels` might represent a
value between 200 and 300 pixels, and an `Interval Float Meters` might represent
a value between 2.5 and 3.7 centimeters. (As with the `Quantity` type, the
`units` type parameter refers to the _base_ unit of a particular quantity type;
lengths in meters, centimeters, feet, miles etc. are all internally stored in
meters.)

-}
type Interval number units
    = Interval ( Quantity number units, Quantity number units )


{-| Construct a zero-width interval containing a single value.

    Interval.singleton (Length.meters 3)
    --> Interval.fromEndpoints
    -->     ( Length.meters 3
    -->     , Length.meters 3
    -->     )

-}
singleton : Quantity number units -> Interval number units
singleton value =
    Interval ( value, value )


{-| Construct an interval from its endpoints (the minimum and maximum values of
the interval).

    highwayCarSpeeds =
        Interval.fromEndpoints
            ( Speed.kilometersPerHour 90
            , Speed.kilometersPerHour 130
            )

The two values should be given in order but will be swapped if
necessary to ensure a valid interval is returned:

    Interval.endpoints <|
        Interval.fromEndpoints
            ( Speed.kilometersPerHour 130
            , Speed.kilometersPerHour 90
            )
    --> ( Speed.kilometersPerHour 90
    --> , Speed.kilometersPerHour 130
    --> )

-}
fromEndpoints : ( Quantity number units, Quantity number units ) -> Interval number units
fromEndpoints givenEndpoints =
    let
        ( Quantity a, Quantity b ) =
            givenEndpoints
    in
    if a <= b then
        Interval givenEndpoints

    else
        Interval ( Quantity b, Quantity a )


{-| Construct an interval containing the two given values (which can be provided
in either order).

    -- "The heights of people participating in the study
    -- ranged from 1.2 to 1.9 meters"
    heightRange =
        Interval.from
            (Length.meters 1.2)
            (Length.meters 1.9)

    -- "Please allow 4 to 6 weeks for delivery"
    estimatedShippingTime =
        Interval.from
            (Duration.weeks 4)
            (Duration.weeks 6)

-}
from : Quantity number units -> Quantity number units -> Interval number units
from (Quantity a) (Quantity b) =
    if a <= b then
        Interval ( Quantity a, Quantity b )

    else
        Interval ( Quantity b, Quantity a )


{-| Find the interval containing one or more input values, by passing the first
value as the first argument and then a list of all other values as the second
argument. For example, to find the interval containing the values 5 cm, 2 cm,
3 cm and 4 cm:

    Interval.hull
        (Length.centimeters 5)
        [ Length.centimeters 3
        , Length.centimeters 2
        , Length.centimeters 4
        ]
    --> Interval.from
    -->     (Length.centimeters 2)
    -->     (Length.centimeters 5)

Why pass the first and all other values as separate arguments? It lets this
function return an `Interval` instead of a `Maybe Interval`. If there was just a
single list as an argument, then this function would have to handle the case of
an empty list being passed by returning `Nothing`. As a result, when using this
function you often end up using it within a `case` expression:

    case values of
        [] ->
            -- some default behavior

        first :: rest ->
            let
                interval =
                    Interval.hull first rest
            in
            -- normal behavior using 'interval'

If you do want the simpler behavior of taking a single list and returning a
`Maybe Interval`, check out [`hullN`](#hullN).

-}
hull : Quantity number units -> List (Quantity number units) -> Interval number units
hull (Quantity x0) rest =
    hullHelp x0 x0 rest


hullHelp : number -> number -> List (Quantity number units) -> Interval number units
hullHelp a b values =
    case values of
        (Quantity x) :: rest ->
            hullHelp (min a x) (max b x) rest

        [] ->
            Interval ( Quantity a, Quantity b )


{-| Construct an interval containing the three given values;

    Interval.hull3 a b c

is equivalent to

    Interval.hull a [ b, c ]

but is more efficient. (If you're looking for a `hull2` function, [`from`](#from)
should do what you want.)

-}
hull3 :
    Quantity number units
    -> Quantity number units
    -> Quantity number units
    -> Interval number units
hull3 (Quantity a) (Quantity b) (Quantity c) =
    Interval
        ( Quantity (min a (min b c))
        , Quantity (max a (max b c))
        )


{-| Attempt to construct an interval containing all _N_ values in the given
list. If the list is empty, returns `Nothing`. If you know you have at least one
value, you can use [`hull`](#hull) instead.

    Interval.hullN
        [ Duration.hours 2
        , Duration.hours 1
        , Duration.hours 3
        ]
    --> Just <|
    -->     Interval.from
    -->         (Duration.hours 1)
    -->         (Duration.hours 3)

    Interval.hullN [ Duration.hours 5]
    --> Just (Interval.singleton (Duration.hours 5))

    Interval.hullN []
    --> Nothing

-}
hullN : List (Quantity number units) -> Maybe (Interval number units)
hullN values =
    case values of
        first :: rest ->
            Just (hull first rest)

        [] ->
            Nothing


{-| Like [`hull`](#hull), but lets you work on any kind of item as long as a
`Quantity` of some kind can be extracted from it. For example, if you had

    type alias Person =
        { name : String
        , age : Duration
        }

then given some people you could find their range of ages as an `Interval Float
Seconds` using

    Interval.hullOf .age
        firstPerson
        [ secondPerson
        , thirdPerson
        , fourthPerson
        ]

See also [`hullOfN`](#hullOfN).

-}
hullOf : (a -> Quantity number units) -> a -> List a -> Interval number units
hullOf getValue first rest =
    let
        (Quantity x0) =
            getValue first
    in
    hullOfHelp x0 x0 getValue rest


hullOfHelp : number -> number -> (a -> Quantity number units) -> List a -> Interval number units
hullOfHelp a b getValue list =
    case list of
        first :: rest ->
            let
                (Quantity x) =
                    getValue first
            in
            hullOfHelp (min a x) (max b x) getValue rest

        [] ->
            Interval ( Quantity a, Quantity b )


{-| Combination of [`hullOf`](#hullOf) and [`hullN`](#hullN).
-}
hullOfN : (a -> Quantity number units) -> List a -> Maybe (Interval number units)
hullOfN getValue items =
    case items of
        first :: rest ->
            Just (hullOf getValue first rest)

        [] ->
            Nothing


{-| Construct an interval containing both of the given intervals.

    firstInterval =
        Interval.from (Length.feet 1) (Length.feet 2)

    secondInterval =
        Interval.from (Length.feet 3) (Length.feet 6)

    Interval.union firstInterval secondInterval
    --> Interval.from (Length.feet 1) (Length.feet 6)

(Note that this is not strictly speaking a 'union' in the precise mathematical
sense, since the result will contain values that are _in between_ the two given
intervals and not actually _in_ either of them if those two intervals do not
overlap.)

-}
union : Interval number units -> Interval number units -> Interval number units
union (Interval ( Quantity a1, Quantity b1 )) (Interval ( Quantity a2, Quantity b2 )) =
    Interval ( Quantity (min a1 a2), Quantity (max b1 b2) )


{-| Attempt to construct an interval containing all the values common to both
given intervals. If the intervals do not intersect, returns `Nothing`.

    Interval.intersection
        (Interval.from (Mass.grams 1) (Mass.grams 3))
        (Interval.from (Mass.grams 2) (Mass.grams 5))
    --> Just (Interval.from (Mass.grams 2) (Mass.grams 3))

    Interval.intersection
        (Interval.from (Mass.grams 1) (Mass.grams 3))
        (Interval.from (Mass.grams 4) (Mass.grams 7))
    --> Nothing

If the two intervals just touch, a singleton interval will be returned:

    Interval.intersection
        (Interval.from (Mass.grams 1) (Mass.grams 3))
        (Interval.from (Mass.grams 3) (Mass.grams 5))
    --> Just (Interval.singleton (Mass.grams 3))

-}
intersection : Interval number units -> Interval number units -> Maybe (Interval number units)
intersection (Interval ( Quantity a1, Quantity b1 )) (Interval ( Quantity a2, Quantity b2 )) =
    let
        maxA =
            max a1 a2

        minB =
            min b1 b2
    in
    if maxA <= minB then
        Just (Interval ( Quantity maxA, Quantity minB ))

    else
        Nothing


{-| Construct an interval containing one or more given intervals:

    Interval.aggregate
        (Interval.singleton (Length.feet 2))
        [ Interval.from
            (Length.feet 3)
            (Length.feet 4)
        ]
    --> Interval.from (Length.feet 2) (Length.feet 4)

Works much like [`hull`](#hull). See also [`aggregateN`](#aggregateN).

-}
aggregate : Interval number units -> List (Interval number units) -> Interval number units
aggregate (Interval ( Quantity a, Quantity b )) rest =
    aggregateHelp a b rest


aggregateHelp : number -> number -> List (Interval number units) -> Interval number units
aggregateHelp a b intervals =
    case intervals of
        (Interval ( Quantity c, Quantity d )) :: rest ->
            aggregateHelp (min a c) (max b d) rest

        [] ->
            Interval ( Quantity a, Quantity b )


{-| Special case of [`aggregate`](#aggregate) for the case of three intervals;

    Interval.aggregate3 first second third

is equivalent to

    Interval.aggregate first [ second, third ]

but is more efficient. (If you're looking for an `aggregate2` function,
[`union`](#union) should do what you want.)

-}
aggregate3 :
    Interval number units
    -> Interval number units
    -> Interval number units
    -> Interval number units
aggregate3 (Interval ( Quantity a1, Quantity b1 )) (Interval ( Quantity a2, Quantity b2 )) (Interval ( Quantity a3, Quantity b3 )) =
    Interval
        ( Quantity (min a1 (min a2 a3))
        , Quantity (max b1 (max b2 b3))
        )


{-| Attemp to construct an interval containing all of the intervals in the given
list. If the list is empty, returns `Nothing`. If you know you have at least one
interval, you can use [`aggregate`](#aggregate) instead.
-}
aggregateN : List (Interval number units) -> Maybe (Interval number units)
aggregateN intervals =
    case intervals of
        first :: rest ->
            Just (aggregate first rest)

        [] ->
            Nothing


{-| Like [`aggregate`](#aggregate), but lets you work on any kind of item as
long as an interval can be generated from it (similar to [`hullOf`](#hullOf)).
-}
aggregateOf : (a -> Interval number units) -> a -> List a -> Interval number units
aggregateOf getInterval first rest =
    let
        (Interval ( Quantity a, Quantity b )) =
            getInterval first
    in
    aggregateOfHelp a b getInterval rest


aggregateOfHelp : number -> number -> (a -> Interval number units) -> List a -> Interval number units
aggregateOfHelp a b getInterval items =
    case items of
        first :: rest ->
            let
                (Interval ( Quantity c, Quantity d )) =
                    getInterval first
            in
            aggregateOfHelp (min a c) (max b d) getInterval rest

        [] ->
            Interval ( Quantity a, Quantity b )


{-| Combination of [`aggregateOf`](#aggregateOf) and [`aggregateN`](#aggregateN).
-}
aggregateOfN : (a -> Interval number units) -> List a -> Maybe (Interval number units)
aggregateOfN getInterval items =
    case items of
        first :: rest ->
            Just (aggregateOf getInterval first rest)

        [] ->
            Nothing


{-| Get the endpoints of an interval (its minimum and maximum values) as a
tuple. The first value will always be less than or equal to the second.

    ( minValue, maxValue ) =
        Interval.endpoints someInterval

For any interval,

    Interval.endpoints interval

is equivalent to (but more efficient than)

    ( Interval.minValue interval
    , Interval.maxValue interval
    )

-}
endpoints : Interval number units -> ( Quantity number units, Quantity number units )
endpoints (Interval intervalEndpoints) =
    intervalEndpoints


{-| Get the minimum value of an interval.
-}
minValue : Interval number units -> Quantity number units
minValue (Interval ( a, _ )) =
    a


{-| Get the maximum value of an interval.
-}
maxValue : Interval number units -> Quantity number units
maxValue (Interval ( _, b )) =
    b


{-| Get the midpoint of an interval.

    Interval.midpoint <|
        Interval.from
            (Duration.hours 2)
            (Duration.hours 3)
    --> Duration.hours 2.5

-}
midpoint : Interval Float units -> Quantity Float units
midpoint (Interval ( Quantity a, Quantity b )) =
    Quantity (a + 0.5 * (b - a))


{-| Get the width of an interval.

    Interval.width <|
        Interval.from
            (Length.meters 1.2)
            (Length.meters 1.35)
    --> Length.centimeters 15

-}
width : Interval number units -> Quantity number units
width (Interval ( Quantity a, Quantity b )) =
    Quantity (b - a)


{-| Interpolate between an interval's endpoints based on a parameter value. A
value of 0.0 corresponds to the minimum value of the interval, a value of 0.5
corresponds to its midpoint and a value of 1.0 corresponds to its maximum value:

    lengthInterval =
        Interval.from (Length.meters 1) (Length.meters 5)

    Interval.interpolate lengthInterval 0
    --> Length.meters 1

    Interval.interpolate lengthInterval 0.75
    --> Length.meters 4

Values less than 0.0 or greater than 1.0 can be used to extrapolate:

    Interval.interpolate lengthInterval 1.5
    --> Length.meters 7

Note that because of how [`Interval.from`](#from) works, the interpolation is in
fact from the minimum value to the maximum, _not_ "from the first
`Interval.from` argument to the second":

    Interval.interpolate
        (Interval.from
            (Length.meters 0)
            (Length.meters 10)
        )
        0.2
    --> 2

    Interval.interpolate
        (Interval.from
            (Length.meters 10)
            (Length.meters 0)
        )
        0.2
    --> 2 -- not 8!

If you want to interpolate from one number down to another, you can use
[`Quantity.interpolateFrom`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#interpolateFrom)
from the `elm-units` package.

-}
interpolate : Interval Float units -> Float -> Quantity Float units
interpolate (Interval ( a, b )) t =
    Quantity.interpolateFrom a b t


{-| Given an interval and a given value, determine the corresponding
interpolation parameter (the parameter that you would pass to [`interpolate`](#interpolate)
to get the given value):

    Interval.interpolationParameter
        (Interval.from
            (Duration.minutes 10)
            (Duration.minutes 15)
        )
        (Duration.minutes 12)
    --> 0.4

The result will be between 0 and 1 if (and only if) the given value is contained
in the given interval:

    Interval.interpolationParameter
        (Interval.from
            (Duration.minutes 10)
            (Duration.minutes 15)
        )
        (Duration.minutes 18)
    --> 1.6

    Interval.interpolationParameter
        (Interval.from
            (Duration.minutes 10)
            (Duration.minutes 15)
        )
        (Duration.minutes 9)
    --> -0.2

This is the inverse of `interpolate`; for any non-zero-width `interval`,

    Interval.interpolationParameter interval value
        |> Interval.interpolate interval

should be equal to the original `value` (within numerical roundoff).

-}
interpolationParameter : Interval Float units -> Quantity Float units -> Float
interpolationParameter (Interval ( Quantity a, Quantity b )) (Quantity x) =
    if a < b then
        (x - a) / (b - a)

    else if x < a then
        -1 / 0

    else if x > b then
        1 / 0

    else
        -- value, a and intervalMaxValue are all equal
        0


{-| Check if an interval contains a given value:

    angleInterval =
        Interval.from
            (Angle.degrees -10)
            (Angle.degrees 30)

    angleInterval |> Interval.contains (Angle.degrees 0)
    --> True

    angleInterval |> Interval.contains (Angle.degrees 45)
    --> False

The minimum and maximum values of an interval are considered to be contained in
the interval (but be careful of numerical roundoff):

    angleInterval |> Interval.contains (Angle.degrees 30)
    --> True

-}
contains : Quantity number units -> Interval number units -> Bool
contains (Quantity x) (Interval ( Quantity a, Quantity b )) =
    a <= x && x <= b


{-| Check if two intervals touch or overlap (have any values in common).

    distanceInterval =
        Interval.from
            (Length.kilometers 5)
            (Length.kilometers 10)

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 8)
                (Length.kilometers 12)
            )
    --> True

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 12)
                (Length.kilometers 15)
            )
    --> False

Intervals that just touch each other are considered to intersect (this is
consistent with `intersection` which will return a zero-width interval for the
intersection of two just-touching intervals):

    distanceInterval
        |> Interval.intersects
            (Interval.from
                (Length.kilometers 10)
                (Length.kilometers 15)
            )
    --> True

-}
intersects : Interval number units -> Interval number units -> Bool
intersects (Interval ( Quantity a1, Quantity b1 )) (Interval ( Quantity a2, Quantity b2 )) =
    a1 <= b2 && b1 >= a2


{-| Check if the second interval is fully contained in the first.

    angleInterval =
        Interval.from
            (Angle.degrees -30)
            (Angle.degrees 30)

    Interval.from (Angle.degrees -5) (Angle.degrees 15)
        |> Interval.isContainedIn angleInterval
    --> True

    Interval.from (Angle.degrees 15) (Angle.degrees 45)
        |> Interval.isContainedIn angleInterval
    --> False

Be careful with the argument order! If not using the `|>` operator, the first
example would be written as:

    Interval.isContainedIn angleInterval
        (Interval.from
            (Angle.degrees -5)
            (Angle.degrees 15)
        )
    --> True

-}
isContainedIn : Interval number units -> Interval number units -> Bool
isContainedIn (Interval ( Quantity a1, Quantity b1 )) (Interval ( Quantity a2, Quantity b2 )) =
    a1 <= a2 && b2 <= b1


{-| Check if the interval is a singleton (the minimum and maximum values are the
same).

    Interval.isSingleton <|
        Interval.from (Length.meters 2) (Length.meters 2)
    --> True

    Interval.isSingleton <|
        Interval.from (Length.meters 2) (Length.meters 3)
    --> False

-}
isSingleton : Interval number units -> Bool
isSingleton (Interval ( a, b )) =
    a == b


{-| Negate an interval. Note that this will flip the order of the endpoints.

    Interval.negate <|
        Interval.from
            (Angle.degrees 20)
            (Angle.degrees 30)
    --> Interval.from
    -->     (Angle.degrees -30)
    -->     (Angle.degrees -20)

-}
negate : Interval number units -> Interval number units
negate (Interval ( Quantity a, Quantity b )) =
    Interval ( Quantity -b, Quantity -a )


{-| Add the given amount to an interval:

    lengthInterval =
        Interval.from
            (Length.meters 2)
            (Length.meters 3)

    lengthInterval |> Interval.add (Length.centinmeters 20)
    --> Interval.from
    -->     (Length.meters 2.2)
    -->     (Length.meters 3.2)

-}
add : Quantity number units -> Interval number units -> Interval number units
add (Quantity delta) (Interval ( Quantity a, Quantity b )) =
    Interval
        ( Quantity (a + delta)
        , Quantity (b + delta)
        )


{-| Subtract the given amount from an interval.

    angleInterval =
        Interval.from
            (Angle.degrees -10)
            (Angle.degrees 50)

    angleInterval |> Interval.subtract (Angle.degrees 30)
    --> Interval.from
    -->     (Angle.degrees -40)
    -->     (Angle.degrees 20)

-}
subtract : Quantity number units -> Interval number units -> Interval number units
subtract (Quantity delta) (Interval ( Quantity a, Quantity b )) =
    Interval ( Quantity (a - delta), Quantity (b - delta) )


{-| Multiply an interval by a given value:

    Interval.multiplyBy 5 <|
        Interval.from
            (Duration.minutes 20)
            (Duration.minutes 30)
    --> Interval.from
    -->     (Duration.minutes 100)
    -->     (Duration.minutes 150)

Note that this will flip the order of the interval's endpoints if the given
value is negative:

    Interval.multiplyBy -2 <|
        Interval.from
            (Angle.degrees 20)
            (Angle.degrees 30)
    --> Interval.from
    -->     (Angle.degrees -60)
    -->     (Angle.degrees -40)

-}
multiplyBy : number -> Interval number units -> Interval number units
multiplyBy scale (Interval ( Quantity a, Quantity b )) =
    if scale >= 0 then
        Interval ( Quantity (a * scale), Quantity (b * scale) )

    else
        Interval ( Quantity (b * scale), Quantity (a * scale) )


{-| Divide an interval by a given value:

    Interval.divideBy 2 <|
        Interval.from (Length.feet 2) (Length.feet 3)
    --> Interval.from (Length.feet 1) (Length.feet 1.5)

Note that this will flip the order of the interval's endpoints if the given
value is negative:

    Interval.divideBy -2 <|
        Interval.from
            (Angle.degrees 20)
            (Angle.degrees 30)
    --> Interval.from
    -->     (Angle.degrees -15)
    -->     (Angle.degrees -10)

-}
divideBy : Float -> Interval Float units -> Interval Float units
divideBy divisor (Interval ( Quantity a, Quantity b )) =
    if divisor == 0 then
        Interval ( Quantity.negativeInfinity, Quantity.positiveInfinity )

    else if divisor > 0 then
        Interval ( Quantity (a / divisor), Quantity (b / divisor) )

    else
        Interval ( Quantity (b / divisor), Quantity (a / divisor) )


{-| Shorthand for `multiplyBy 0.5`.
-}
half : Interval Float units -> Interval Float units
half (Interval ( Quantity a, Quantity b )) =
    Interval ( Quantity (0.5 * a), Quantity (0.5 * b) )


{-| Shorthand for `multiplyBy 2`.
-}
twice : Interval number units -> Interval number units
twice (Interval ( Quantity a, Quantity b )) =
    Interval ( Quantity (2 * a), Quantity (2 * b) )


{-| Add two intervals together.

    firstInterval =
        Interval.from (Length.feet 5) (Length.feet 10)

    secondInterval =
        Interval.from (Length.feet 2) (Length.feet 3)

    firstInterval |> Interval.plus secondInterval
    --> Interval.from (Length.feet 7) (Length.feet 13)

-}
plus : Interval number units -> Interval number units -> Interval number units
plus (Interval ( Quantity a2, Quantity b2 )) (Interval ( Quantity a1, Quantity b1 )) =
    Interval ( Quantity (a2 + a1), Quantity (b2 + b1) )


{-| Subtract the first interval from the second. This means that `minus` makes
the most sense when using `|>`:

    firstInterval =
        Interval.from (Length.feet 5) (Length.feet 10)

    secondInterval =
        Interval.from (Length.feet 2) (Length.feet 3)

    firstInterval |> Interval.minus secondInterval
    --> Interval.from (Length.feet 2) (Length.feet 8)

Without the pipe operator, the above would be written as:

    Interval.minus secondInterval firstInterval
    --> Interval.from 2 8

-}
minus : Interval number units -> Interval number units -> Interval number units
minus (Interval ( Quantity a2, Quantity b2 )) (Interval ( Quantity a1, Quantity b1 )) =
    Interval ( Quantity (a1 - b2), Quantity (b1 - a2) )


{-| Multiply the second interval by the first. The order only matters if the
two intervals have different units (since it will affect the units of the
result) but, like other functions in this module, `times` is generally designed
to be used with `|>`:

    width =
        Interval.from (Length.centimeters 10) (Length.centimeters 12)

    height =
        Interval.from (Length.centimeters 5) (Length.centimeters 6)

    width |> Interval.times height
    --> Interval.from
    -->     (Area.squareCentimeters 50)
    -->     (Area.squareCentimeters 72)

-}
times : Interval number units2 -> Interval number units1 -> Interval number (Product units1 units2)
times (Interval ( Quantity a2, Quantity b2 )) (Interval ( Quantity a1, Quantity b1 )) =
    let
        aa =
            a1 * a2

        ab =
            a1 * b2

        ba =
            b1 * a2

        bb =
            b1 * b2
    in
    Interval
        ( Quantity (min (min (min aa ab) ba) bb)
        , Quantity (max (max (max aa ab) ba) bb)
        )


{-| Get the absolute value of an interval.

    Interval.abs <|
        Interval.from
            (Length.meters -3)
            (Length.meters 2)
    --> Interval.from (Length.meters 0) (Length.meters 3)

-}
abs : Interval number units -> Interval number units
abs interval =
    let
        (Interval ( Quantity a, Quantity b )) =
            interval
    in
    if a >= 0 then
        interval

    else if b <= 0 then
        negate interval

    else
        Interval ( Quantity.zero, Quantity (max -a b) )


{-| Get the square of an interval.
-}
squared : Interval number units -> Interval number (Squared units)
squared interval =
    let
        (Interval ( Quantity a, Quantity b )) =
            interval
    in
    if a >= 0 then
        Interval ( Quantity (a * a), Quantity (b * b) )

    else if b <= 0 then
        Interval ( Quantity (b * b), Quantity (a * a) )

    else if -a < b then
        Interval ( Quantity.zero, Quantity (b * b) )

    else
        Interval ( Quantity.zero, Quantity (a * a) )


{-| Get the cube of an interval.
-}
cubed : Interval number units -> Interval number (Cubed units)
cubed interval =
    let
        (Interval ( Quantity a, Quantity b )) =
            interval
    in
    Interval ( Quantity (a * a * a), Quantity (b * b * b) )


{-| Create a [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for quantities within a given interval. For example,

    Interval.randomValue <|
        Interval.from (Length.meters 5) (Length.meters 10)

is a `Generator` that will produce random lengths between 5 and 10 meters.

-}
randomValue : Interval Float units -> Generator (Quantity Float units)
randomValue (Interval ( Quantity a, Quantity b )) =
    Random.map Quantity (Random.float a b)
