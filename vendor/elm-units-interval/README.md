# elm-units-interval

Copied from https://github.com/ianmackenzie/elm-units-interval/tree/2.3.0.

This package lets you work with ranges of values (intervals) based on the
`Quantity` type from [`elm-units`][elm-units]. You can do things like:

- Check if a given value is within an interval
- Construct intervals from lists of values or other intervals
- Find the intersection of two intervals
- Perform [arithmetic][interval-arithmetic] on intervals

```elm
import Quantity.Interval as Interval exposing (Interval)
import Angle exposing (Radians)
import Pixels exposing (Pixels)

angleRange : Interval Float Radians
angleRange =
    Interval.from (Angle.degrees 0) (Angle.degrees 180)

widthRange : Interval Int Pixels
widthRange =
    Interval.from (Pixels.int 100) (Pixels.int 300)
```

Various functionality is included for constructing intervals (including as the
union or intersection of other intervals) and checking for overlap, intersection
or containment:

```elm
import Quantity.Interval as Interval exposing (Interval)
import Length
import Duration

distanceInterval =
    Interval.from (Length.meters 10) (Length.meters 20)

Interval.endpoints distanceInterval
--> ( Length.meters 10, Length.meters 20 )

Interval.hull
    (Length.feet 5)
    [ Length.feet 3
    , Length.feet 2
    , Length.feet 4
    ]
--> Interval.from (Length.feet 2) (Length.feet 5)

Interval.union
    (Interval.from
        (Duration.minutes 1)
        (Duration.minutes 2)
    )
    (Interval.from
        (Duration.minutes 3)
        (Duration.minutes 5)
    )
--> Interval.from (Duration.minutes 1) (Duration.minutes 5)

Interval.intersection
    (Interval.from
        (Duration.hours 1)
        (Duration.hours 3)
    )
    (Interval.from
        (Duration.hours 2)
        (Duration.hours 5)
    )
--> Just
-->     (Interval.from
-->         (Duration.hours 2)
-->         (Duration.hours 3)
-->     )

Interval.from (Angle.radians 0) (Angle.radians pi)
    |> Interval.contains (Angle.degrees 90)
--> True

Interval.from (Angle.radians 0) (Angle.radians pi)
    |> Interval.contains (Angle.degrees 270)
--> False
```

Most functionality is contained in the [`Quantity.Interval`](Quantity-Interval)
module, but some functionality specific to particular kinds of intervals is
contained in [`Angle.Interval`](Angle-Interval) and [`Temperature.Interval`](Temperature-Interval).

[elm-interval]: https://package.elm-lang.org/packages/ianmackenzie/elm-interval/latest/
[elm-units]: https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/
[interval-arithmetic]: https://en.wikipedia.org/wiki/Interval_arithmetic
