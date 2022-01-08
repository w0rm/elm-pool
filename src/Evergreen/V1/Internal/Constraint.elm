module Evergreen.V1.Internal.Constraint exposing (..)

import Evergreen.V1.Internal.Shape
import Evergreen.V1.Internal.Vector3


type Constraint coordinates
    = PointToPoint Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3
    | Hinge Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3
    | Lock Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3 Evergreen.V1.Internal.Vector3.Vec3
    | Distance Float


type alias ConstraintGroup = 
    { bodyId1 : Int
    , bodyId2 : Int
    , constraints : (List (Constraint Evergreen.V1.Internal.Shape.CenterOfMassCoordinates))
    }