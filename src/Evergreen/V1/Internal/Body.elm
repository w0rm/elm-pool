module Evergreen.V1.Internal.Body exposing (..)

import Evergreen.V1.Internal.Material
import Evergreen.V1.Internal.Matrix3
import Evergreen.V1.Internal.Shape
import Evergreen.V1.Internal.Transform3d
import Evergreen.V1.Internal.Vector3
import Evergreen.V1.Physics.Coordinates


type alias Body data = 
    { id : Int
    , data : data
    , material : Evergreen.V1.Internal.Material.Material
    , transform3d : (Evergreen.V1.Internal.Transform3d.Transform3d Evergreen.V1.Physics.Coordinates.WorldCoordinates 
    { defines : Evergreen.V1.Internal.Shape.CenterOfMassCoordinates
    })
    , centerOfMassTransform3d : (Evergreen.V1.Internal.Transform3d.Transform3d Evergreen.V1.Physics.Coordinates.BodyCoordinates 
    { defines : Evergreen.V1.Internal.Shape.CenterOfMassCoordinates
    })
    , velocity : Evergreen.V1.Internal.Vector3.Vec3
    , angularVelocity : Evergreen.V1.Internal.Vector3.Vec3
    , mass : Float
    , shapes : (List (Evergreen.V1.Internal.Shape.Shape Evergreen.V1.Internal.Shape.CenterOfMassCoordinates))
    , worldShapes : (List (Evergreen.V1.Internal.Shape.Shape Evergreen.V1.Physics.Coordinates.WorldCoordinates))
    , force : Evergreen.V1.Internal.Vector3.Vec3
    , torque : Evergreen.V1.Internal.Vector3.Vec3
    , boundingSphereRadius : Float
    , linearDamping : Float
    , angularDamping : Float
    , invMass : Float
    , invInertia : Evergreen.V1.Internal.Matrix3.Mat3
    , invInertiaWorld : Evergreen.V1.Internal.Matrix3.Mat3
    }