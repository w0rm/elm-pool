module Evergreen.V1.Internal.Shape exposing (..)

import Evergreen.V1.Internal.Vector3
import Evergreen.V1.Shapes.Convex
import Evergreen.V1.Shapes.Plane
import Evergreen.V1.Shapes.Sphere


type CenterOfMassCoordinates
    = CenterOfMassCoordinates


type Shape coordinates
    = Convex Evergreen.V1.Shapes.Convex.Convex
    | Plane Evergreen.V1.Shapes.Plane.Plane
    | Sphere Evergreen.V1.Shapes.Sphere.Sphere
    | Particle Evergreen.V1.Internal.Vector3.Vec3