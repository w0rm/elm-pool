module Evergreen.V3.Internal.Constraint exposing (..)

import Evergreen.V3.Internal.Shape
import Evergreen.V3.Internal.Vector3


type Constraint coordinates
    = PointToPoint Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3
    | Hinge Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3
    | Lock Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3 Evergreen.V3.Internal.Vector3.Vec3
    | Distance Float


type alias ConstraintGroup = 
    { bodyId1 : Int
    , bodyId2 : Int
    , constraints : (List (Constraint Evergreen.V3.Internal.Shape.CenterOfMassCoordinates))
    }