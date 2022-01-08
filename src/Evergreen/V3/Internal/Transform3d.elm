module Evergreen.V3.Internal.Transform3d exposing (..)

import Evergreen.V3.Internal.Vector3


type Orientation3d
    = Orientation3d Float Float Float Float


type Transform3d coordinates defines
    = Transform3d Evergreen.V3.Internal.Vector3.Vec3 Orientation3d