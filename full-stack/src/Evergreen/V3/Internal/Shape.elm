module Evergreen.V3.Internal.Shape exposing (..)

import Evergreen.V3.Internal.Vector3
import Evergreen.V3.Shapes.Convex
import Evergreen.V3.Shapes.Plane
import Evergreen.V3.Shapes.Sphere


type CenterOfMassCoordinates
    = CenterOfMassCoordinates


type Shape coordinates
    = Convex Evergreen.V3.Shapes.Convex.Convex
    | Plane Evergreen.V3.Shapes.Plane.Plane
    | Sphere Evergreen.V3.Shapes.Sphere.Sphere
    | Particle Evergreen.V3.Internal.Vector3.Vec3