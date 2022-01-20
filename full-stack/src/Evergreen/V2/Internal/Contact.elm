module Evergreen.V2.Internal.Contact exposing (..)

import Evergreen.V2.Internal.Body
import Evergreen.V2.Internal.Vector3


type alias Contact = 
    { ni : Evergreen.V2.Internal.Vector3.Vec3
    , pi : Evergreen.V2.Internal.Vector3.Vec3
    , pj : Evergreen.V2.Internal.Vector3.Vec3
    }


type alias ContactGroup data = 
    { body1 : (Evergreen.V2.Internal.Body.Body data)
    , body2 : (Evergreen.V2.Internal.Body.Body data)
    , contacts : (List Contact)
    }