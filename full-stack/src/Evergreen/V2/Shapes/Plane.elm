module Evergreen.V2.Shapes.Plane exposing (..)

import Evergreen.V2.Internal.Vector3


type alias Plane = 
    { normal : Evergreen.V2.Internal.Vector3.Vec3
    , position : Evergreen.V2.Internal.Vector3.Vec3
    }