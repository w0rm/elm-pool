module Evergreen.V1.Internal.World exposing (..)

import Array
import Evergreen.V1.Internal.Body
import Evergreen.V1.Internal.Constraint
import Evergreen.V1.Internal.Contact
import Evergreen.V1.Internal.Vector3


type alias World data = 
    { bodies : (List (Evergreen.V1.Internal.Body.Body data))
    , constraints : (List Evergreen.V1.Internal.Constraint.ConstraintGroup)
    , freeIds : (List Int)
    , nextBodyId : Int
    , gravity : Evergreen.V1.Internal.Vector3.Vec3
    , contactGroups : (List (Evergreen.V1.Internal.Contact.ContactGroup data))
    , simulatedBodies : (Array.Array (Evergreen.V1.Internal.Body.Body data))
    }


type Protected data
    = Protected (World data)