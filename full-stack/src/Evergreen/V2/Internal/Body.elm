module Evergreen.V2.Internal.Body exposing (..)

import Evergreen.V2.Internal.Material
import Evergreen.V2.Internal.Matrix3
import Evergreen.V2.Internal.Shape
import Evergreen.V2.Internal.Transform3d
import Evergreen.V2.Internal.Vector3
import Evergreen.V2.Physics.Coordinates


type alias Body data = 
    { id : Int
    , data : data
    , material : Evergreen.V2.Internal.Material.Material
    , transform3d : (Evergreen.V2.Internal.Transform3d.Transform3d Evergreen.V2.Physics.Coordinates.WorldCoordinates 
    { defines : Evergreen.V2.Internal.Shape.CenterOfMassCoordinates
    })
    , centerOfMassTransform3d : (Evergreen.V2.Internal.Transform3d.Transform3d Evergreen.V2.Physics.Coordinates.BodyCoordinates 
    { defines : Evergreen.V2.Internal.Shape.CenterOfMassCoordinates
    })
    , velocity : Evergreen.V2.Internal.Vector3.Vec3
    , angularVelocity : Evergreen.V2.Internal.Vector3.Vec3
    , mass : Float
    , shapes : (List (Evergreen.V2.Internal.Shape.Shape Evergreen.V2.Internal.Shape.CenterOfMassCoordinates))
    , worldShapes : (List (Evergreen.V2.Internal.Shape.Shape Evergreen.V2.Physics.Coordinates.WorldCoordinates))
    , force : Evergreen.V2.Internal.Vector3.Vec3
    , torque : Evergreen.V2.Internal.Vector3.Vec3
    , boundingSphereRadius : Float
    , linearDamping : Float
    , angularDamping : Float
    , invMass : Float
    , invInertia : Evergreen.V2.Internal.Matrix3.Mat3
    , invInertiaWorld : Evergreen.V2.Internal.Matrix3.Mat3
    }