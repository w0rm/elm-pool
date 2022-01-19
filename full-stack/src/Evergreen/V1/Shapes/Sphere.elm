module Evergreen.V1.Shapes.Sphere exposing (..)

import Evergreen.V1.Internal.Matrix3
import Evergreen.V1.Internal.Vector3


type alias Sphere = 
    { radius : Float
    , position : Evergreen.V1.Internal.Vector3.Vec3
    , volume : Float
    , inertia : Evergreen.V1.Internal.Matrix3.Mat3
    }