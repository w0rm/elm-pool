module Evergreen.V2.Internal.Time exposing (..)

import Duration
import Quantity


type AbsoluteTime
    = AbsoluteTime


type alias Absolute = (Quantity.Quantity Float AbsoluteTime)


type alias Duration = Duration.Duration