module Evergreen.V3.Shapes.Plane exposing (..)

import Evergreen.V3.Internal.Vector3


type alias Plane = 
    { normal : Evergreen.V3.Internal.Vector3.Vec3
    , position : Evergreen.V3.Internal.Vector3.Vec3
    }