module Evergreen.V3.Internal.World exposing (..)

import Array
import Evergreen.V3.Internal.Body
import Evergreen.V3.Internal.Constraint
import Evergreen.V3.Internal.Contact
import Evergreen.V3.Internal.Vector3


type alias World data = 
    { bodies : (List (Evergreen.V3.Internal.Body.Body data))
    , constraints : (List Evergreen.V3.Internal.Constraint.ConstraintGroup)
    , freeIds : (List Int)
    , nextBodyId : Int
    , gravity : Evergreen.V3.Internal.Vector3.Vec3
    , contactGroups : (List (Evergreen.V3.Internal.Contact.ContactGroup data))
    , simulatedBodies : (Array.Array (Evergreen.V3.Internal.Body.Body data))
    }


type Protected data
    = Protected (World data)