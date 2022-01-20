module Evergreen.V3.Internal.Body exposing (..)

import Evergreen.V3.Internal.Material
import Evergreen.V3.Internal.Matrix3
import Evergreen.V3.Internal.Shape
import Evergreen.V3.Internal.Transform3d
import Evergreen.V3.Internal.Vector3
import Evergreen.V3.Physics.Coordinates


type alias Body data = 
    { id : Int
    , data : data
    , material : Evergreen.V3.Internal.Material.Material
    , transform3d : (Evergreen.V3.Internal.Transform3d.Transform3d Evergreen.V3.Physics.Coordinates.WorldCoordinates 
    { defines : Evergreen.V3.Internal.Shape.CenterOfMassCoordinates
    })
    , centerOfMassTransform3d : (Evergreen.V3.Internal.Transform3d.Transform3d Evergreen.V3.Physics.Coordinates.BodyCoordinates 
    { defines : Evergreen.V3.Internal.Shape.CenterOfMassCoordinates
    })
    , velocity : Evergreen.V3.Internal.Vector3.Vec3
    , angularVelocity : Evergreen.V3.Internal.Vector3.Vec3
    , mass : Float
    , shapes : (List (Evergreen.V3.Internal.Shape.Shape Evergreen.V3.Internal.Shape.CenterOfMassCoordinates))
    , worldShapes : (List (Evergreen.V3.Internal.Shape.Shape Evergreen.V3.Physics.Coordinates.WorldCoordinates))
    , force : Evergreen.V3.Internal.Vector3.Vec3
    , torque : Evergreen.V3.Internal.Vector3.Vec3
    , boundingSphereRadius : Float
    , linearDamping : Float
    , angularDamping : Float
    , invMass : Float
    , invInertia : Evergreen.V3.Internal.Matrix3.Mat3
    , invInertiaWorld : Evergreen.V3.Internal.Matrix3.Mat3
    }