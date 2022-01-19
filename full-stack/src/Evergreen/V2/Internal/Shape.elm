module Evergreen.V2.Internal.Shape exposing (..)

import Evergreen.V2.Internal.Vector3
import Evergreen.V2.Shapes.Convex
import Evergreen.V2.Shapes.Plane
import Evergreen.V2.Shapes.Sphere


type CenterOfMassCoordinates
    = CenterOfMassCoordinates


type Shape coordinates
    = Convex Evergreen.V2.Shapes.Convex.Convex
    | Plane Evergreen.V2.Shapes.Plane.Plane
    | Sphere Evergreen.V2.Shapes.Sphere.Sphere
    | Particle Evergreen.V2.Internal.Vector3.Vec3