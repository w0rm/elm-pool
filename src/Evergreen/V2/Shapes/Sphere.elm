module Evergreen.V2.Shapes.Sphere exposing (..)

import Evergreen.V2.Internal.Matrix3
import Evergreen.V2.Internal.Vector3


type alias Sphere = 
    { radius : Float
    , position : Evergreen.V2.Internal.Vector3.Vec3
    , volume : Float
    , inertia : Evergreen.V2.Internal.Matrix3.Mat3
    }