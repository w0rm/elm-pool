module Evergreen.V1.Shapes.Plane exposing (..)

import Evergreen.V1.Internal.Vector3


type alias Plane = 
    { normal : Evergreen.V1.Internal.Vector3.Vec3
    , position : Evergreen.V1.Internal.Vector3.Vec3
    }