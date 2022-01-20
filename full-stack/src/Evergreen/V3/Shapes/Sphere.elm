module Evergreen.V3.Shapes.Sphere exposing (..)

import Evergreen.V3.Internal.Matrix3
import Evergreen.V3.Internal.Vector3


type alias Sphere = 
    { radius : Float
    , position : Evergreen.V3.Internal.Vector3.Vec3
    , volume : Float
    , inertia : Evergreen.V3.Internal.Matrix3.Mat3
    }