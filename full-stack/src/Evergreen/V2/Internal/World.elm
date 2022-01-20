module Evergreen.V2.Internal.World exposing (..)

import Array
import Evergreen.V2.Internal.Body
import Evergreen.V2.Internal.Constraint
import Evergreen.V2.Internal.Contact
import Evergreen.V2.Internal.Vector3


type alias World data = 
    { bodies : (List (Evergreen.V2.Internal.Body.Body data))
    , constraints : (List Evergreen.V2.Internal.Constraint.ConstraintGroup)
    , freeIds : (List Int)
    , nextBodyId : Int
    , gravity : Evergreen.V2.Internal.Vector3.Vec3
    , contactGroups : (List (Evergreen.V2.Internal.Contact.ContactGroup data))
    , simulatedBodies : (Array.Array (Evergreen.V2.Internal.Body.Body data))
    }


type Protected data
    = Protected (World data)